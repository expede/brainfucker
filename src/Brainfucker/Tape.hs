-- Tape metaphor for "memory"
module Brainfucker.Tape(Cell, Tape, tapeStart) where

import qualified Data.List.Zipper as Z (Zipper(Zip))

type Cell = Int
type Tape = Z.Zipper Cell

-- | Starting position of a Tape.
-- Has one '0' to the left of the cursor
-- This will get filled in on first run of runMachine, anyway
--
-- >>> tapeStart
-- Zip [0] [0]
tapeStart :: Tape
tapeStart = Z.Zip [0] [0]
