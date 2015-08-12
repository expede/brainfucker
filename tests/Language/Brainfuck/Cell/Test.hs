module Language.Brainfuck.Cell.Test (cellSuite) where

import Test.Tasty
import qualified Language.Brainfuck.Cell.QuickCheck as QC

cellSuite :: TestTree
cellSuite = testGroup "Cell Suite" [QC.tests]
