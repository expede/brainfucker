{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}

{-|
Module      : Brainfuck interpreter
Description : Run the Brainfuck AST
-}

module Language.Brainfuck ( Tape
                          , interpret
                          , toAST
                          , start
                          ) where

import Language.Brainfuck.Tape
import Language.Brainfuck.Lex (toAST)
import Language.Brainfuck.AST (AST, Bfk(..))

import Control.Monad      (liftM)
import Control.Monad.Free (Free(..))

-- | Interpret a Brainfuck `AST`, "running" the virtual machine
interpret :: AST () -> IO Tape -> IO Tape
interpret (Pure _)  tape = tape
interpret (Free xs) tape = case xs of
  End        -> tape
  TapeL   ys -> interpret ys $ liftM (<<#) tape
  TapeR   ys -> interpret ys $ liftM (#>>) tape
  IncCell ys -> interpret ys $ liftM (#++) tape
  DecCell ys -> interpret ys $ liftM (#--) tape
  GetCell ys -> do
    t <- tape
    (^#^) t
    interpret ys (return t)
  SetCell ys -> do
    putStrLn "Insert character to overwrite: "
    newChar <- getChar
    let newTape = liftM (>#< newChar) tape
    interpret ys newTape
  Loop zs ys -> do
    t <- tape
    case cursor t of
      0 -> interpret ys (return t)
      _ -> interpret (Free xs) (interpret zs (return t))
