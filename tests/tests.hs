{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brainfucker
import Test.Hspec
import qualified Data.Map as M

main :: IO ()
main = hspec $ do
  describe "Convert characters to commands" $ do
    it "is awesome"
