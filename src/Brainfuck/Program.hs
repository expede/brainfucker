{-# LANGUAGE NoImplicitPrelude #-}

module Brainfuck.Program
  ( Program
  , Opcode (..)
  , findLoopEnd
  ) where

import           ClassyPrelude
import           Data.Vector           (Vector)
import           Data.Vector           ((!?))

import           Brainfuck.Cell        (Math)
import           Brainfuck.Control     (Control (..))
import           Brainfuck.Interaction (Communication)
import           Brainfuck.Pointer     (Pointer)
import           Brainfuck.Tape        (Move)

type Program = Vector Opcode

data Opcode
  = Cell Math
  | Tape Move
  | Loop Control
  | Interaction Communication
  deriving Show

findLoopEnd :: Program -> Pointer -> Pointer
findLoopEnd program = go 0
  where
    go :: Word8 -> Pointer -> Pointer
    go depth counter =
      case program !? counter of
        Nothing           -> error $ show counter <> " loop is missing a `]`!"

        Just (Loop End)   -> if depth == 0
                              then counter
                              else go (pred depth) (succ counter)

        Just (Loop Begin) -> go (succ depth) (succ counter)
        _                 -> go depth        (succ counter)
