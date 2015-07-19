{-# LANAGUGE TypeSynonymInstances #-}

module Language.Brainfuck.AST.Test (astSuite) where

import Language.Brainfuck.AST

import Test.Tasty ( TestTree
                  , testGroup
                  )

import Test.Tasty.QuickCheck ( Gen
                             , Arbitrary
                             , CoArbitrary
                             , testProperty
                             , forAll
                             , suchThat
                             , arbitrary
                             )

import Test.Properties (idempotent)

instance Arbitrary (Bfk a)
-- instance CoArbitrary (Bfk a)

astSuite :: TestTree
astSuite = testGroup "Tape"
  [ -- testProperty "`set` is idempotent" $
      -- (end >> end >> tapeL) == (end >> tapeL)
  -- , testProperty "`left` and `right` commute for all tapes" $
  --     forAll tapes $ commutes left right
  -- , testProperty "`left` and `right` invert for all tapes" $
  --     forAll tapes $ inverts left right
  ]

commutes :: Eq a => (a -> a) -> (a -> a) -> a -> Bool
commutes f g a = (f . g) a == (g . f) a
