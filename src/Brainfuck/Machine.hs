{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Brainfuck.Machine
  ( Machine (..)
  , blank
  , cellPtr
  , jumpStack
  , pc
  , pred
  , program
  , initProgram
  , tape
  ) where

import           ClassyPrelude
import           Control.Lens      (makeLenses, (&), (.~))

import           Brainfuck.Pointer (Pointer)
import           Brainfuck.Program (Program)
import           Brainfuck.Tape    (Tape)

data Machine = Machine
  { _program   :: Program
  , _pc        :: Pointer
  , _tape      :: Tape
  , _cellPtr   :: Pointer
  , _jumpStack :: [Pointer]
  } deriving Show

makeLenses ''Machine

initProgram :: Program -> Machine
initProgram pgm = blank & program .~ pgm

blank :: Machine
blank = Machine
  { _pc        = 0
  , _cellPtr   = 0
  , _jumpStack = []
  , _program   = empty
  , _tape      = replicate 30000 0
  }
