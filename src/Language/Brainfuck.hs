{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Brainfuck interpreter
Description : Run the Brainfuck AST
-}

module Language.Brainfuck ( Tape
                          , interpret
                          , toAST
                          , start
                          , (###)
                          ) where

import Language.Brainfuck.Tape
import Language.Brainfuck.Lex (toAST)
import Language.Brainfuck.AST (AST, Bfk(..), unend)
import Control.Monad.Free     (Free(..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text
-- >>> let helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." :: Text
-- >>> let hd = "++++>>>+++" :: Text

-- | Interpret a Brainfuck `AST`, "running" the virtual machine
--
-- >>> interpret (toAST helloWorld) start
-- Hello World!
interpret :: AST () -> Tape -> IO ()
interpret     (Pure _)  _    = return ()
interpret ast@(Free xs) tape = case xs of
  End        -> return ()
  TapeL   ys -> interpret ys $ (<<#) tape
  TapeR   ys -> interpret ys $ (#>>) tape
  IncCell ys -> interpret ys $ (#++) tape
  DecCell ys -> interpret ys $ (#--) tape
  GetCell ys -> (^#^) tape >> interpret ys tape
  SetCell ys -> do
    putStrLn "Insert character to overwrite: "
    newChar <- getChar
    interpret ys $ newChar >#< tape
  Loop zs ys -> case (#) tape of
    0 -> interpret ys tape
    _ -> interpret (unend zs >> ast) tape
