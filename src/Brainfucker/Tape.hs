-- Tape metaphor for "memory"
module Brainfucker.Tape(Cell, Tape, tapeStart) where

import qualified Data.List.Zipper as Z (Zipper(Zip))

type Cell = Int
type Tape = Z.Zipper Cell

tapeStart :: Tape
tapeStart = Z.Zip [0] [0]
