{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Brainfuck interpreter
Description : Read in and run the Brainfuck AST
-}

module Language.Brainfuck.Interpret
       ( Tape
       , interpret
       , interpret'
       , toAST
       , start
       , (###)
       ) where

import Language.Brainfuck.Tape
import Language.Brainfuck.Lex (toAST)
import Language.Brainfuck.AST (AST, Bfk(..), (.>))

import Control.Monad.Free     (Free(..))
import Data.Text (Text)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text
-- >>> let helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." :: Text
-- >>> let helloWorldHard = ">++++++++[<+++++++++>-]<.>>+>+>++>[-]+<[>[->+<<++++>]<<]>.+++++++..+++.>>+++++++.<<<[[-]<[-]>]<+++++++++++++++.>>.+++.------.--------.>>+.>++++."
-- >>> let hd = "++++>>>+++" :: Text

-- | Interpret straight Brainfuck text, and "run" the virtual machine with a clean tape
--
-- >>> interpret helloWorld
-- Hello World!
--
-- >>> interpret helloWorldHard
-- Hello World!
interpret :: Text -> IO ()
interpret text = interpret' (toAST text) (###)

-- | Interpret a Brainfuck `AST`, "running" the virtual machine
--
-- >>> interpret' (toAST helloWorld) start
-- Hello World!
--
-- >>> interpret' (toAST helloWorldHard) start
-- Hello World!
interpret' :: AST () -> Tape -> IO ()
interpret'     (Pure _)  _    = return ()
interpret' ast@(Free xs) tape = case xs of
  TapeL   ys -> interpret' ys $ (<<#) tape
  TapeR   ys -> interpret' ys $ (#>>) tape
  IncCell ys -> interpret' ys $ (#++) tape
  DecCell ys -> interpret' ys $ (#--) tape
  GetCell ys -> (^#^) tape >> interpret' ys tape
  SetCell ys -> do
    putStrLn "Insert character to overwrite: "
    newChar <- getChar
    interpret' ys $ newChar >#< tape
  Loop zs ys -> case (#) tape of
    0 -> interpret' ys tape
    _ -> interpret' (zs .> ast) tape
