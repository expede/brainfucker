{-# LANGUAGE OverloadedStrings #-}
module Brainfucker.MachineSpec(main, spec) where

import Test.Hspec
import qualified Brainfucker.Machine as B

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "lexChar" $
    it "converts `Char`s to `Lexeme`s" $
      B.lexChar '<' `shouldBe` B.TapeLeft
