{-# LANGUAGE NoImplicitPrelude #-}

import           Brainfuck
import           ClassyPrelude

main :: IO ()
main = do
  path    <- show  <$> getLine
  machine <- setup <$> parseFile path
  run machine
