{-# LANGUAGE FlexibleInstances #-}
module Language.Brainfuck.Tape.QuickCheck (tests) where

import Language.Brainfuck.Tape
import Data.List.Zipper (Zipper(Zip))

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
middle (Zip as bs) = take 100 as ++ take 100 bs

instance {-# OVERLAPS #-} Arbitrary Tape where
  arbitrary = do
    list1 <- genListCell
    list2 <- genListCell
    return $ Zip list1 list2
    where genListCell = infiniteList $ choose (0, 255)
