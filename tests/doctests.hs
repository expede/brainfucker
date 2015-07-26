module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-isrc"
               , "src/Main.hs"
               , "src/Language/Brainfuck/AST.hs"
               , "src/Language/Brainfuck/Lex.hs"
               , "src/Language/Brainfuck/Tape.hs"
               , "src/Language/Brainfuck/Interpret.hs"
               ]
