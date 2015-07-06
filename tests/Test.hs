{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import Test.Tasty ( defaultMain
                  , testGroup
                  , TestTree
                  )

import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series

import Language.Brainfuck.Tape.Test
import Language.Brainfuck.AST

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
          [ tapeSuite
          -- , lexSuite
          -- , astSuite
          ]
