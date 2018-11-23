{-# LANGUAGE NoImplicitPrelude #-}

module Brainfuck.Program
  ( Program
  , Opcode (..)
  , findLoopEnd
  ) where

import           ClassyPrelude
import           Data.Vector           (Vector, (!?))

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
    go depth pointer =
      case program !? pointer of
        Nothing           -> error $ show pointer <> " loop is missing a `]`!"

        Just (Loop End)   -> if depth == 0
                               then pointer
                               else go (pred depth) (succ pointer)

        Just (Loop Begin) -> go (succ depth) (succ pointer)
        _                 -> go depth        (succ pointer)
