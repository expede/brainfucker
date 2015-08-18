{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, TypeSynonymInstances, FlexibleInstances #-}

{-|
Module      : AST
Description : The abstract syntax tree, and convenience functions for construction
-}

module Language.Brainfuck.AST ( AST
                              , Bfk(..)
                              , Free(..)
                              , tapeL
                              , tapeR
                              , incCell
                              , decCell
                              , getCell
                              , setCell
                              , loopStart
                              , subtree
                              , suspend
                              , (.>)
                              , sequence'
                              ) where

import Control.Monad.Free (Free(..), liftF)
import Test.QuickCheck (Gen, Arbitrary(..), sized, choose, vectorOf)

-- | Description of all Brainfuck commands. `a` is the nested remainder of the tree.
data Bfk a = End         -- ^ End of computation. Only here for completeness.
           | TapeL   a   -- ^ "Move tape left"
           | TapeR   a   -- ^ "Move tape right"
           | IncCell a   -- ^ "Increment cell (tape head) by one"
           | DecCell a   -- ^ "Decrement cell (tape head) by one"
           | GetCell a   -- ^ "Retrieve the value of the tape's head"
           | SetCell a   -- ^ "Overwrite the tape's head"
           | Loop    a a -- ^ A subtree to either execute or skip
             deriving ( Show
                      , Eq
                      , Functor
                      , Foldable
                      )

instance Arbitrary a => Arbitrary (Bfk a) where
  arbitrary = do
    n <- choose (0, 7) :: Gen Int
    a <- arbitrary
    b <- arbitrary
    return $ case n of
                  0 -> End
                  1 -> TapeL   a
                  2 -> TapeR   a
                  3 -> IncCell a
                  4 -> DecCell a
                  5 -> GetCell a
                  6 -> SetCell a
                  7 -> Loop    a b

-- | Wrap `Bfk` in a Free Monad
type AST a = Free Bfk a
instance Arbitrary a => Arbitrary (AST a) where
  arbitrary = do
    bfk <- arbitrary
    return $ Free bfk

{- | Suspended `TapeL`, and easily combined with `Bfk` helpers with `>>`

>>> tapeL
Free (TapeL (Pure ()))

>>> tapeL .> tapeL .> tapeL
Free (TapeL (Free (TapeL (Free (TapeL (Pure ()))))))
-}
tapeL :: AST ()
tapeL = liftF $ TapeL ()

{- | Suspended `TapeR`, and easily combined with `Bfk` helpers with `>>`

>>> tapeR
Free (TapeR (Pure ()))

>>> tapeR .> tapeR .> tapeR
Free (TapeR (Free (TapeR (Free (TapeR (Pure ()))))))
-}
tapeR :: AST ()
tapeR = liftF $ TapeR ()

{- | Suspended `incCell`, and easily combined with `Bfk` helpers with `>>`

>>> incCell
Free (IncCell (Pure ()))

>>> incCell .> incCell .> incCell
Free (IncCell (Free (IncCell (Free (IncCell (Pure ()))))))
-}
incCell :: AST ()
incCell = liftF $ IncCell ()

{- | Suspended `decCell`, and easily combined with `Bfk` helpers with `>>`

>>> decCell
Free (DecCell (Pure ()))

>>> decCell .> decCell .> incCell
Free (DecCell (Free (DecCell (Free (IncCell (Pure ()))))))
-}
decCell :: AST ()
decCell = liftF $ DecCell ()

{- | Suspended `getCell`, and easily combined with `Bfk` helpers with `>>`

>>> getCell
Free (GetCell (Pure ()))

>>> getCell .> getCell .> incCell
Free (GetCell (Free (GetCell (Free (IncCell (Pure ()))))))
-}
getCell :: AST ()
getCell = liftF $ GetCell ()

{- | Suspended `setCell`, and easily combined with `Bfk` helpers with `>>`

>>> setCell
Free (SetCell (Pure ()))

>>> setCell .> setCell .> incCell
Free (SetCell (Free (SetCell (Free (IncCell (Pure ()))))))
-}
setCell :: AST ()
setCell = liftF $ SetCell ()

{- | Start a #Loop#, with both branches open.
Free's `>>` behaves like `fmap`, so it will append a `Bfk` command to both sides.
`subtree` is availble as a practical work-around.

>>> loopStart >> setCell
Free (Loop (Free (SetCell (Pure ()))) (Free (SetCell (Pure ()))))
-}
loopStart :: AST ()
loopStart = liftF $ Loop () ()

-- | Construct the subtree of a #Loop#
--
-- >>> subtree tapeL
-- Free (Loop (Free (TapeL (Pure ()))) (Pure ()))
subtree :: AST () -> AST ()
subtree ast = Free $ Loop ast (Pure ())

{- | "Right append": append to the main spine of an AST (no inner subtrees)

tapeR
  \*
  tapeL
    \*
    subtree
    /      \*
  tapeR     tapeL
    \          \*
   incCell     subtree
               /      \*
             subtree  printCell
             /     \          .
           tapeL   tapeL      . (.>) (decCell >> decCell)
                              .
                             decCell
                                \
                               decCell

>>> tapeR .> subtree tapeL
Free (TapeR (Free (Loop (Free (TapeL (Pure ()))) (Pure ()))))
-}
infixl 1 .>
(.>) :: AST () -> AST () -> AST ()
Pure _  .> ast = ast
Free xs .> ast = case xs of
  End        -> ast
  TapeL   as -> Free $ TapeL   (as .> ast)
  TapeR   as -> Free $ TapeR   (as .> ast)
  IncCell as -> Free $ IncCell (as .> ast)
  DecCell as -> Free $ DecCell (as .> ast)
  GetCell as -> Free $ GetCell (as .> ast)
  SetCell as -> Free $ SetCell (as .> ast)
  Loop bs as -> Free $ Loop bs (as .> ast)

-- | Sequence'
--
-- >>> sequence' [tapeL, tapeR]
-- Free (TapeL (Free (TapeR (Pure ()))))
sequence' :: [AST ()] -> AST ()
sequence' = foldr (.>) (Pure ())

{- | Simply `Pure ()`. `AST`'s "zero" element.

>>> suspend
Pure ()

>>> tapeL == (tapeL .> suspend)
True
-}
suspend :: AST ()
suspend = Pure ()
