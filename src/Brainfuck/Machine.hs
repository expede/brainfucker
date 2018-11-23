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
