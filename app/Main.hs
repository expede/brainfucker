{-# LANGUAGE NoImplicitPrelude #-}

import           Brainfuck
import           ClassyPrelude

main :: IO ()
main = do
  path    <- show <$> getLine
  program <- parseFile path
  run $ initProgram program
