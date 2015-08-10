{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}

{-|
Module      : Cell
Description : Emulate an individual ASCII memory cell
-}

module Language.Brainfuck.Cell ( Cell(unCell)
                               , toCell
                               , mapCell
                               ) where

-- | Cell is a single memory cell in the Tape
newtype Cell = Cell { unCell :: Int }
  deriving (Show, Eq, Ord)

toCell :: Int -> Cell
toCell int = Cell $ int `mod` 255

mapCell :: (Int -> Int) -> Cell -> Cell
mapCell func = toCell . func . unCell
