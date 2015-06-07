module Main where

import Test.Tasty ( defaultMain
                  , testGroup
                  , TestTree
                  )

import Language.Brainfuck.Tape.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
          [ tapeSuite
          -- , lexSuite
          -- , astSuite
          ]
