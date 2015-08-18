{-# LANGUAGE TypeSynonymInstances #-}

module Language.Brainfuck.AST.Test (astSuite) where

import Language.Brainfuck.AST

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

instance Arbitrary (Bfk a)
-- instance CoArbitrary (Bfk a)

astSuite :: TestTree
astSuite = testGroup "AST"
  [ testRightCompose
  ]

testRightCompose :: TestTree
testRightCompose = testGroup "(.>)"
  [ testCase "no loops: append normally" $
      (tapeL .> tapeR) @?= Free (TapeL (Free (TapeR (Pure ()))))
  ]

prop_normal_append = ast1 .> ast2 == ast1 >> ast2
  where ast1 = gen noLoopAST
        ast2 = gen noLoopAST

noLoopAST = choose [incCell, decCell, tapeL, tapeR, getCell, setCell]
-- testSequence' = ()

-- >>> (subtree tapeL) .> tapeR
-- Free (Loop (Free (TapeL (Pure ()))) (Free (TapeR (Pure ()))))

-- >>> tapeR .> subtree tapeL
-- Free (TapeR (Free (Loop (Free (TapeL (Pure ()))) (Pure ()))))

-- >>> tapeR .> subtree tapeL .> incCell
-- Free (TapeR (Free (Loop (Free (TapeL (Pure ()))) (Free (IncCell (Pure ()))))))


{-
-}
