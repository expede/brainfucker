{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Brainfuck.Machine
  ( Machine (..)
  , blank
  , cellPtr
  , jumpStack
  , pc
  , pred
  , program
  , setup
  , tape
  , jumpToLoopEnd
  ) where

import           ClassyPrelude
import           Control.Lens      (makeLenses, (&), (.~))
import           Data.Vector       ((!?))

import           Brainfuck.Control (Control (..))
import           Brainfuck.Pointer (Pointer)
import           Brainfuck.Program (Opcode (..), Program)
import           Brainfuck.Tape    (Tape)

data Machine = Machine
  { _program   :: Program
  , _pc        :: Pointer
  , _tape      :: Tape
  , _cellPtr   :: Pointer
  , _jumpStack :: [Pointer]
  }
  deriving Show

makeLenses ''Machine

setup :: Program -> Machine
setup pgm = blank & program .~ pgm

blank :: Machine
blank = Machine
  { _pc        = 0
  , _cellPtr   = 0
  , _jumpStack = []
  , _program   = empty
  , _tape      = replicate 30000 0
  }

jumpToLoopEnd ::  Machine -> Machine
jumpToLoopEnd m@(Machine {..}) = go 0 _pc
  where go :: Word8 -> Pointer -> Machine
        go depth counter =
          case _program !? counter of
            Nothing -> error $ show counter <> " loop is missing a `]`!"

            Just (Loop End) ->
              if depth == 0
                then m & pc .~ counter
                else go (pred depth) (succ counter)

            Just (Loop Begin) -> go (succ depth) (succ counter)
            _                 -> go depth        (succ counter)
