module Main where

import Test.Tasty ( defaultMain
                  , testGroup
                  , localOption
                  , TestTree
                  )

import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series

import Language.Brainfuck.Interpret.Test

main :: IO ()
main = defaultMain tests

-- tests :: TestTree
tests = localOption (QC.QuickCheckTests 9999) $ testGroup "All Tests"
          [
            -- tapeSuite
          -- , lexSuite
          -- , astSuite
          interpretSuite
          ]
