{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}

-- Module      : Cell
-- Description : Emulate an individual ASCII memory cell
module Language.Brainfuck.Cell ( Cell(unCell)
                               , toCell
                               , cellChr
                               , chrToCell
                               , liftC
                               ) where

import Data.Char       (chr, ord)
import Test.QuickCheck (Arbitrary(..), choose)

-- | Cell is a single memory cell in the Tape
newtype Cell = Cell { unCell :: Int }
  deriving (Eq, Ord)

instance Show Cell where
  show t = show . chr $ unCell t

instance Arbitrary Cell where
  arbitrary = toCell <$> choose (0, 255)

{- | Converts an Int to a Cell and enforces the modular number system
>>> toCell 7 < toCell 6
False

>>> toCell 256 < toCell 6
True
-}
toCell :: Int -> Cell
toCell int = Cell $ int `mod` 255

-- | Store a `Char` as a `Cell`
-- >>> chrToCell 'X' :: Cell
-- 'X'
chrToCell :: Char -> Cell
chrToCell = toCell . ord

-- | Retrieve a `Char` from a `Cell`
-- >>> cellChr $ chrToCell 'X' :: Char
-- 'X'
cellChr :: Cell -> Char
cellChr = chr . unCell

{-| Lift an integer operation into the `Cell`'s modular number system
>>> liftC (\x -> x - 1) $ chrToCell 'Z'
'Y'

>>> unCell . liftC (+ 1) $ toCell 254
0
-}
liftC :: (Int -> Int) -> Cell -> Cell
liftC func = toCell . func . unCell
