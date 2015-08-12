module Language.Brainfuck.Tape.QuickCheck (tests) where

import Language.Brainfuck.Tape

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Tape"
  [ testProperty "(>#<) is idempotent" $ prop_idempotent . (>#<)

  , testProperty "(#++) commutes with (#--)"    $ prop_commute (#++) (#--)
  , testProperty "(#++) mutually inverts (#--)" $ prop_mutual_invert (#++) (#--)

  , testProperty "(#>>) commutes with (#<<)"    $ prop_commute (#>>) (<<#)
  , testProperty "(#>>) mutually inverts (<<#)" $ prop_mutual_invert (#>>) (<<#)
  ]

prop_idempotent :: (Tape -> Tape) -> Tape -> Bool
prop_idempotent f a = (show . f) a == (show . f . f) a

prop_commute :: (Tape -> Tape) -> (Tape -> Tape) -> Tape -> Bool
prop_commute f g a = (show . f . g) a == (show . g . f) a

prop_invert :: (Tape -> Tape) -> (Tape -> Tape) -> Tape -> Bool
prop_invert f g z = (show . f . g) z == show z

prop_mutual_invert :: (Tape -> Tape) -> (Tape -> Tape) -> Tape -> Bool
prop_mutual_invert f g z = prop_invert f g z && prop_invert g f z
