{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.Brainfucker.AST ( AST
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
                            ) where

import Control.Monad.Free ( Free(..)
                          , liftF
                          )

data Bfk a = TapeL   a
           | TapeR   a
           | IncCell a
           | DecCell a
           | GetCell a
           | SetCell a
           | Loop    a a
           | End
             deriving ( Show
                      , Eq
                      , Functor
                      , Foldable
                      , Traversable
                      )

type AST a = Free Bfk a

tapeL :: AST ()
tapeL = liftF $ TapeL ()

tapeR :: AST ()
tapeR = liftF $ TapeR ()

incCell :: AST ()
incCell = liftF $ IncCell ()

decCell :: AST ()
decCell = liftF $ DecCell ()

getCell :: AST ()
getCell = liftF $ GetCell ()

setCell :: AST ()
setCell = liftF $ SetCell ()

-- NB: Free's (>>) behaves like fmap.
-- >>> loopStart >> setCell
-- >>> => Free (Loop (Free (SetCell (Free (Pure ())))) (Free (SetCell (Free (Pure ())))))
loopStart :: AST ()
loopStart = liftF $ Loop () ()

subtree :: AST () -> AST ()
subtree ast = Free $ Loop (ast >> end) (Pure ())

end :: AST ()
end = liftF End

suspend :: AST ()
suspend = Pure ()

-- Totally optional, makes reading easier sometimes
loopEnd :: AST ()
loopEnd = end
