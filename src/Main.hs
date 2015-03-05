module Main where

import qualified Brainfucker as B

main :: IO ()
main = do putStrLn "Path to input brainfuck file: "
          path <- getLine
          file <- readFile path
          B.interpret file
          print (0 :: Int)
