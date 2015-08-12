{-# LANGUAGE FlexibleInstances #-}
module Language.Brainfuck.Tape.QuickCheck (tests) where

import Language.Brainfuck.Cell
import Language.Brainfuck.Tape

import Data.List.Zipper
import Control.Monad

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding (infiniteList)
import Test.QuickCheck.Instances.List (infiniteList)

tests :: TestTree
tests = testGroup "Tape"
  [ testProperty "(>#<) is idempotent" $ prop_idempotent . (>#<)
  , testProperty "(#++) inverts (#--)" $ prop_commute (#++) (#--)
  , testProperty "(#>>) inverts (<<#)" $ prop_inverts (#>>) (<<#)
  ]

prop_idempotent :: (Tape -> Tape) -> Tape -> Bool
prop_idempotent f a = (middle . f) a == (middle . f . f) a

prop_commute :: (Tape -> Tape) -> (Tape -> Tape) -> Tape -> Bool
prop_commute f g a = (middle . f . g) a == (middle . g . f) a

prop_inverts :: (Tape -> Tape) -> (Tape -> Tape) -> Tape -> Bool
prop_inverts f g z = (middle . f . g) z == middle z

middle :: Tape -> [Cell]
middle tape = joinHeads zipper
  where zipper = unTape tape
        joinHeads (Zip as bs) = take 100 as ++ take 100 bs
