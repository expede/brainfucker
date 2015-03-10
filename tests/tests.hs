{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Brainfucker
import Test.Hspec
-- import qualified Data.Map as M

main :: IO ()
main = hspec .
  describe "Convert characters to commands" $
    it "is awesome" $
      head [1] `shouldBe` (1 :: Int)
