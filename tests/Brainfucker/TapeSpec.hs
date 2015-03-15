{-# LANGUAGE OverloadedStrings #-}
module Brainfucker.TapeSpec(main, spec) where

import Test.Hspec
import qualified Brainfucker.Tape as T
import qualified Data.List.Zipper as Z

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tapeStart" $ do
    it "has one 0 before the cursor" $
      (Z.cursor $ Z.left T.tapeStart) `shouldBe` (0 :: Int)
    it "has 0 at the cursor" $
      Z.cursor T.tapeStart `shouldBe` (0 :: Int)
    it "has nothing after the cursor" $
      (Z.endp $ Z.right T.tapeStart) `shouldBe` True
