{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Brainfuck
  ( setup
  , run
  , parseFile
  ) where

import           ClassyPrelude         hiding (unpack)
import           Control.Arrow         ((>>>))
import           Control.Lens          hiding (cons)
import           Data.Char             (chr, ord)
import           Data.Monoid           (Sum (..))
import           Data.Vector           ((!?))

import           Brainfuck.Cell
import           Brainfuck.Control
import           Brainfuck.Interaction
import           Brainfuck.Machine
import           Brainfuck.Parser      (parseFile)
import           Brainfuck.Program
import           Brainfuck.Tape

run :: MonadIO m => Machine -> m ()
run machine =
  step machine >>= \case
    Just m  -> return m >>= run
    Nothing -> do
      putStrLn ""
      putStrLn "====="
      putStrLn "Done!"

step :: MonadIO m => Machine -> m (Maybe Machine)
step m@(Machine { .. }) =
  case _program !? _pc of
    Just opcode -> Just <$> interpret m opcode
    Nothing     -> return Nothing

interpret :: MonadIO m => Machine -> Opcode -> m Machine
interpret m@(Machine { .. }) = \case
  Cell math  -> m & tape . ix _cellPtr %~ stepCell math & prep
  Tape move  -> m & cellPtr %~ stepTape move & prep

  Interaction Overwrite -> do
    input <- getChar
    m & tape . ix _cellPtr .~ (fromIntegral $ ord input)
      & prep

  Interaction Print -> do
    putChar . chr . fromIntegral . getSum $ m ^. tape . ix _cellPtr
    prep m

  Loop ctrl  ->
    case (ctrl, _tape !? _cellPtr) of
      (_,     Nothing) -> error $ show _cellPtr <> " is not a cell location on the tape!!"
      (Begin, Just 0)  -> m & jumpToLoopEnd & prep
      (Begin, _)       -> m & jumpStack %~ cons _pc & prep
      (End,   Just 0)  -> m & jumpStack %~ tailEx & prep
      (End,   _)       ->
        case _jumpStack of
          []             -> error "Program error: nowhere left to jump back to!"
          (location : _) -> m & pc .~ location & prep

prep :: MonadIO m => Machine -> m Machine
prep = pc %~ succ >>> pure
