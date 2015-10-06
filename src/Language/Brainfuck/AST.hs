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
                              ) where

import Control.Monad.Free (Free(..), liftF)
import Test.QuickCheck (Gen, Arbitrary(..), choose)

-- | Description of all Brainfuck commands. `a` is the nested remainder of the tree.
data Bfk a = TapeL   a   -- ^ "Move tape left"
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
    n <- choose (0, 6) :: Gen Int
    a <- arbitrary
    b <- arbitrary
    return $ case n of
                  0 -> TapeL   a
                  1 -> TapeR   a
                  2 -> IncCell a
                  3 -> DecCell a
                  4 -> GetCell a
                  5 -> SetCell a
                  _ -> Loop    a b

-- | Wrap `Bfk` in a Free Monad
type AST a = Free Bfk a

instance Arbitrary a => Arbitrary (AST a) where
  arbitrary = do
    bfk <- arbitrary
    return $ Free bfk

instance Monoid (AST ()) where
  mempty = return mempty

  xs `mappend` ast = case xs of
    Pure _            -> ast
    Free (Loop bs as) -> Free $ Loop bs (as `mappend` ast)
    Free as           -> Free $ fmap (`mappend` ast) as

  mconcat = foldr mappend mempty

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
