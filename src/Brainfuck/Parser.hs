{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Brainfuck.Parser
  ( parseFile
  , parse
  , tokenize
  ) where

import           ClassyPrelude         hiding (unpack)
import           Data.ByteString.Char8 (unpack)

import           Brainfuck.Cell
import           Brainfuck.Control
import           Brainfuck.Interaction
import           Brainfuck.Program
import           Brainfuck.Tape

parseFile :: MonadIO m => FilePath -> m Program
parseFile path = (parse . unpack) <$> readFile path

parse :: [Char] -> Program
parse = fromList . parse'
  where parse' [] = []
        parse' (c : cs) =
          case tokenize c of
            Just opcode -> (opcode : parse' cs)
            Nothing     -> parse' cs

tokenize :: Char -> Maybe Opcode
tokenize = \case
  '+' -> Just $ Cell Inc
  '-' -> Just $ Cell Dec
  '<' -> Just $ Tape ShiftLeft
  '>' -> Just $ Tape ShiftRight
  '[' -> Just $ Loop Begin
  ']' -> Just $ Loop End
  '.' -> Just $ Interaction Print
  ',' -> Just $ Interaction Overwrite
  _   -> Nothing
