module Language.Brainfuck.Tape.Test (tapeSuite) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit as HU
-- import qualified Test.Tasty.QuickCheck  as QC

import qualified Test.Tasty.SmallCheck  as SC
import           Test.SmallCheck.Series (Serial)

import Language.Brainfuck.Tape

tapeSuite :: TestTree
tapeSuite = testGroup "Tape"
  [ SC.testProperty "SC Tape.cursor is idempotent" $
      isSucc start
  ]

-- tapeProps :: TestTree
idempotent f s = f s == f (f s)
isSucc s = (cursor . inc) s == (cursor s) + 1
