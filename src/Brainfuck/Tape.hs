{-# LANGUAGE NoImplicitPrelude #-}

module Brainfuck.Tape
  ( Move (..)
  , Tape
  , stepTape
  ) where

import           ClassyPrelude

import           Brainfuck.Cell    (Cell)
import           Brainfuck.Pointer (Pointer)

type Tape = Vector Cell

data Move = ShiftLeft | ShiftRight
  deriving Show

stepTape :: Move -> Pointer -> Pointer
stepTape ShiftLeft  = pred
stepTape ShiftRight = succ
