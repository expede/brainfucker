{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

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

import Control.Monad.Free ( Free(..)
                          , liftF
                          )

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

-- | Wrap `Bfk` in a Free Monad
type AST a = Free Bfk a

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

{- | Construct the subtree of a #Loop#

>>> subtree tapeL
Free (Loop (Free (TapeL (Pure ()))) (Pure ()))
-}
subtree :: AST () -> AST ()
subtree ast = Free $ Loop ast (Pure ())

-- | "Right compose": compose the main spine of an AST (no iner subtrees)
--
-- >>> tapeL .> tapeR
-- Free (TapeL (Free (TapeR (Pure ()))))
--
-- >>> (subtree tapeL) .> tapeR
-- Free (Loop (Free (TapeL (Pure ()))) (Free (TapeR (Pure ()))))
--
-- >>> tapeR .> subtree tapeL
-- Free (TapeR (Free (Loop (Free (TapeL (Pure ()))) (Pure ()))))
--
-- >>> tapeR .> subtree tapeL .> incCell
-- Free (TapeR (Free (Loop (Free (TapeL (Pure ()))) (Free (IncCell (Pure ()))))))
infixl 1 .>
(.>) :: AST () -> AST () -> AST ()
Pure _  .> ast = ast
Free xs .> ast = Free $ case xs of
  TapeL   as -> TapeL   (as .> ast)
  TapeR   as -> TapeR   (as .> ast)
  IncCell as -> IncCell (as .> ast)
  DecCell as -> DecCell (as .> ast)
  GetCell as -> GetCell (as .> ast)
  SetCell as -> SetCell (as .> ast)
  Loop bs as -> Loop bs (as .> ast)

-- | Sequence'
-- >>> sequence' [tapeL, tapeR]
-- Free (TapeL (Free (TapeR (Pure ()))))
sequence' :: [AST ()] -> AST ()
sequence' = foldr (.>) (Pure ())

{- | Simply `Pure ()`. In effect, this operation acts as the identity.

>>> suspend
Pure ()

>>> tapeL == (tapeL .> suspend)
True
-}
suspend :: AST ()
suspend = Pure ()
