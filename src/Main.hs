module Main where

import System.IO
import Language.Brainfuck
import qualified Data.Text.IO as T

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering

  putStrLn "Enter your brainfuck program: "
  text <- T.getLine
  interpret text
