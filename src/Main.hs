module Main where

import System.IO
import Data.Brainfucker
import qualified Data.Text.IO as T

main :: IO Tape
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

  putStrLn "Enter your brainfuck program: "
  text <- T.getLine
  ast  <- return $ toAST text
  interpret ast $ return start
