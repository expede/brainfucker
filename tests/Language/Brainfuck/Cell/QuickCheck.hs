module Language.Brainfuck.Cell.QuickCheck (tests) where

import Language.Brainfuck.Cell

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Cell"
  [ testProperty "chrToCell inverts cellChr" $ prop_inverts cellChr chrToCell
  , testProperty "unCell inverts toCell"     $ prop_inverts unCell toCell
  ]

prop_inverts :: Eq a => (a -> b) -> (b -> a) -> a -> Bool
prop_inverts f g x = (g . f) x == x
