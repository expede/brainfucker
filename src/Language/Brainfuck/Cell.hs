{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}

-- Module      : Cell
-- Description : Emulate an individual ASCII memory cell
module Language.Brainfuck.Cell ( Cell(unCell)
                               , toCell
                               , cellChr
                               , chrToCell
                               ) where

import Data.Char       (chr, ord)
import Test.QuickCheck (Arbitrary(..), choose)

-- | Cell is a single memory cell in the Tape
newtype Cell = Cell { unCell :: Int }
  deriving ( Eq
           , Ord
           , Num
           , Enum
           )

instance Show Cell where
  show t = show . chr $ unCell t

instance Arbitrary Cell where
  arbitrary = toCell <$> choose (0, 255)

toCell :: Int -> Cell
toCell int = Cell $ int `mod` 255

cellChr :: Cell -> Char
cellChr = chr . unCell

chrToCell :: Char -> Cell
chrToCell = toCell . ord
