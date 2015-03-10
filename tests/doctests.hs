module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Main.hs", "src/Brainfucker.hs", "src/Brainfucker/Tape.hs", "src/Brainfucker/Machine.hs"]
