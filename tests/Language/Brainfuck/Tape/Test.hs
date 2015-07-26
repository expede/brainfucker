module Language.Brainfuck.Tape.Test (tapeSuite) where

import Test.Tasty
import qualified Language.Brainfuck.Tape.QuickCheck as QC

tapeSuite :: TestTree
tapeSuite = testGroup "Tape Suite" [QC.tests]
