{-# LANGUAGE OverloadedStrings #-}
module Brainfucker.MachineSpec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Brainfucker.Machine as B

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lexChar" $ do
    context "converts characters to Lexemes" $ do
      context "Brainfuck characters" $ do
        it "'<' to TapeLeft"      $ B.lexChar '<' `shouldBe` B.TapeLeft
        it "'>' to TapeRight"     $ B.lexChar '>' `shouldBe` B.TapeRight
        it "'+' to IncrementCell" $ B.lexChar '+' `shouldBe` B.IncrementCell
        it "'-' to DecrementCell" $ B.lexChar '-' `shouldBe` B.DecrementCell
        it "'.' to PrintCell"     $ B.lexChar '.' `shouldBe` B.PrintCell
        it "',' to OverwriteCell" $ B.lexChar ',' `shouldBe` B.OverwriteCell
        it "'[' to LoopStart"     $ B.lexChar '[' `shouldBe` B.LoopStart
        it "']' to LoopEnd"       $ B.lexChar ']' `shouldBe` B.LoopEnd

      context "non-Brainfuck characters" $ do
        it "`Ignore`s all other characters" $ do
          B.lexChar 'a' `shouldBe` B.Ignore
          B.lexChar 'Z' `shouldBe` B.Ignore
          B.lexChar '*' `shouldBe` B.Ignore
          B.lexChar '&' `shouldBe` B.Ignore

  describe "toMachine" $ do
    it "returns a shorter or equally-long list" $
      property $ \x -> length (B.toMachine x) <= length x
    it "does not include Ignore" $
      property $ \x -> all (/= B.Command B.Ignore 1) (B.toMachine x)
    it "produces recursive Machines when reading in loops" $
      head (B.toMachine "[+],#.*a") `shouldBe` B.Loop [B.Command B.IncrementCell 1]
