module Brainfuck.Program
  ( Program
  , Opcode (..)
  ) where

import           Data.Vector           (Vector)

import           Brainfuck.Cell        (Math)
import           Brainfuck.Control     (Control)
import           Brainfuck.Interaction (Communication)
import           Brainfuck.Tape        (Move)

type Program = Vector Opcode

data Opcode
  = Cell Math
  | Tape Move
  | Loop Control
  | Interaction Communication
  deriving Show
