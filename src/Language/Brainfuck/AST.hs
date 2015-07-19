{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}

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
                              , loopEnd
                              , end
                              , subtree
                              , suspend
                              , unend
                              ) where
import GHC.Generics

import Control.Monad.Free ( Free(..)
                          , liftF
                          )

-- | Description of all Brainfuck commands. `a` is the nested remainder of the tree.
data Bfk a  = TapeL   a   -- ^ "Move tape left"
           | TapeR   a   -- ^ "Move tape right"
           | IncCell a   -- ^ "Increment cell (tape head) by one"
           | DecCell a   -- ^ "Decrement cell (tape head) by one"
           | GetCell a   -- ^ "Retrieve the value of the tape's head"
           | SetCell a   -- ^ "Overwrite the tape's head"
           | Loop    a a -- ^ A subtree to either execute or skip
           | End         -- ^ "Cease computation" (at least on this branch)
             deriving ( Show
                      , Eq
                      , Functor
                      , Generic
                      )

-- | Wrap `Bfk` in a Free Monad
type AST a = Free Bfk a

{- | Suspended `TapeL`, and easily combined with `Bfk` helpers with `>>`

>>> tapeL
Free (TapeL (Pure ()))

>>> tapeL >> tapeL >> tapeL
Free (TapeL (Free (TapeL (Free (TapeL (Pure ()))))))

>>> tapeL >> end
Free (TapeL (Free End))
-}
tapeL :: AST ()
tapeL = liftF $ TapeL ()

{- | Suspended `TapeR`, and easily combined with `Bfk` helpers with `>>`

>>> tapeR
Free (TapeR (Pure ()))

>>> tapeR >> tapeR >> tapeR
Free (TapeR (Free (TapeR (Free (TapeR (Pure ()))))))

>>> tapeR >> end
Free (TapeR (Free End))
-}
tapeR :: AST ()
tapeR = liftF $ TapeR ()

{- | Suspended `incCell`, and easily combined with `Bfk` helpers with `>>`

>>> incCell
Free (IncCell (Pure ()))

>>> incCell >> incCell >> incCell
Free (IncCell (Free (IncCell (Free (IncCell (Pure ()))))))

>>> incCell >> end
Free (IncCell (Free End))
-}
incCell :: AST ()
incCell = liftF $ IncCell ()

{- | Suspended `decCell`, and easily combined with `Bfk` helpers with `>>`

>>> decCell
Free (DecCell (Pure ()))

>>> decCell >> decCell >> incCell
Free (DecCell (Free (DecCell (Free (IncCell (Pure ()))))))

>>> decCell >> end
Free (DecCell (Free End))
-}
decCell :: AST ()
decCell = liftF $ DecCell ()

{- | Suspended `getCell`, and easily combined with `Bfk` helpers with `>>`

>>> getCell
Free (GetCell (Pure ()))

>>> getCell >> getCell >> incCell
Free (GetCell (Free (GetCell (Free (IncCell (Pure ()))))))

>>> getCell >> end
Free (GetCell (Free End))
-}
getCell :: AST ()
getCell = liftF $ GetCell ()

{- | Suspended `setCell`, and easily combined with `Bfk` helpers with `>>`

>>> setCell
Free (SetCell (Pure ()))

>>> setCell >> setCell >> incCell
Free (SetCell (Free (SetCell (Free (IncCell (Pure ()))))))

>>> setCell >> end
Free (SetCell (Free End))
-}
setCell :: AST ()
setCell = liftF $ SetCell ()

{- | Start a #Loop#, with both branches open.
Free's `>>` behaves like `fmap`, so it will append a `Bfk` command to both sides.
`subtree` is availble as a practical work-around.

>>> loopStart >> setCell
Free (Loop (Free (SetCell (Pure ()))) (Free (SetCell (Pure ()))))

>>> loopStart >> end
Free (Loop (Free End) (Free End))
-}
loopStart :: AST ()
loopStart = liftF $ Loop () ()

{- | Construct the subtree of a #Loop#

>>> subtree tapeL
Free (Loop (Free (TapeL (Free End))) (Pure ()))
-}
subtree :: AST () -> AST ()
-- subtree ast = Free $ Loop (ast >> end) (Pure ())
subtree ast = Free $ Loop (ast >> end) (Pure ())

unend :: AST () -> AST ()
unend (Free End) = Pure ()
unend ast = case ast of
  Free (TapeL xs) -> tapeL >> unend xs
  Free (TapeR xs) -> tapeR >> unend xs
  Free (IncCell xs) -> incCell >> unend xs
  Free (DecCell xs) -> decCell >> unend xs
  Free (GetCell xs) -> getCell >> unend xs
  Free (SetCell xs) -> setCell >> unend xs
  Free (Loop xs ys) -> Free (Loop xs (unend ys))


{- | Finish a branch of a #Bfk# tree. Also, `end >> _ = end`.

>>> end
Free End

>>> end >> end >> end >> end
Free End

>>> tapeL >> end
Free (TapeL (Free End))

>>> end >> tapeL >> tapeL >> loopStart
Free End

prop> (end >> Free End) == Free End
-}
end :: AST ()
end = liftF End

{- | Simply `Pure ()`. In effect, this operation acts as the identity.

>>> suspend
Pure ()

>>> tapeL == (tapeL >> suspend)
True
-}
suspend :: AST ()
suspend = Pure ()

{- | Alternate syntax for `end`. Can make reading easier for #subtree#.

>>> loopEnd
Free End

>>> subtree (tapeL >> loopEnd) >> incCell >> end
Free (Loop (Free (TapeL (Free End))) (Free (IncCell (Free End))))
-}
loopEnd :: AST ()
loopEnd = end
