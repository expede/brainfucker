{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Brainfuck.Cell
  ( Cell
  , Math (..)
  , stepCell
  ) where

import           ClassyPrelude
import           Data.Monoid   (Sum)

type Cell = Sum Word8

data Math = Inc | Dec
  deriving Show

stepCell :: Math -> (Cell -> Cell)
stepCell Inc = fmap $ \case
  255 -> 0
  s   -> s + 1
stepCell Dec = fmap $ \case
  0 -> 255
  s -> s - 1
