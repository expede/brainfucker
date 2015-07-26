module Language.Brainfuck.Tape.QuickCheck (tests) where

import Language.Brainfuck.Tape
import qualified Data.List.Zipper as Z

import Test.Tasty      (TestTree, testGroup)
import Test.Properties (idempotent, inverts)
import Test.Tasty.QuickCheck ( Gen
                             , testProperty
                             , forAll
                             , suchThat
                             , arbitrary
                             )

tests :: TestTree
tests = testGroup "Tape"
  [ testProperty "(>#<) is idempotent" $ idempotent . (>#<)
  , testProperty "(#++) inverts (#--)" $ prop_inverts (#++) (#--)
  , testProperty "(#>>) inverts (<<#)" $ prop_inverts (#>>) (<<#)
  ]

prop_commute :: Eq a => (a -> a) -> (a -> a) -> a -> Bool
prop_commute f g a = (f . g) a == (g . f) a

-- inverts :: Eq a => (a -> a) -> (a -> a) -> a -> Bool
-- inverts f g a = (f . g) a == a

prop_inverts f g = forAll tapes $ f `inverts` g

tapes :: Gen Tape
tapes = arbitrary `suchThat` minSubTape
  where minSubTape (Z.Zip a b) = length a > 2 && length b > 2
