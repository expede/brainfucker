module Main where

import qualified Data.List.Zipper    as Z
import qualified Data.Vector.Generic as V
-- NB: Comments will distinguish between "tape" for memoryspace,
--     and "machine" for command sequence.

newtype Cell    = Int
newtype Tape    = Zipper Cell
newtype Index   = Int
newtype Machine = V.Vector Command

data Command = TapeLeft
             | TapeRight
             | IncrementCell
             | DecrementCell
             | WriteFromCell
             | ReadToCell
             | LoopStart
             | LoopEnd

charToCommand :: Char -> Command
charToCommand c = case c of
  '<' -> TapeLeft
  '>' -> TapeRight
  '+' -> IncrementCell
  '-' -> DecrementCell
  '.' -> WriteFromCell
  ',' -> ReadToCell
  '[' -> LoopStart
  ']' -> LoopEnd
  _   -> error "invalid character"

lex :: String -> Machine
lex = map charToCommand

afterLoopStart :: Index -> Machine -> Index
afterLoopStart pos machine = if machine ! pos == LoopStart
                               then succ pos
                               else findLoopStart (pred pos) machine

afterLoopEnd :: Index -> Machine -> Index
afterLoopEnd pos machine = if machine ! (pred pos) == LoopEnd
                             then pos
                             else indexAfterLoop (succ pos) machine

-- This probably belongs in `main`, for obvious reasons
stepMachine :: Tape -> Command -> (Maybe IO (), Tape)
stepMachine (Z.Zip [] []) command = stepMachine (Z.insert 0 tape) command
stepMachine tape          command = case command of
  TapeLeft       -> Z.left tape
  TapeRight      -> Z.right tape
  IncrementCell  -> Z.replace (succ cell) tape
  DecrementCell  -> Z.replace (pred cell) tape
  WriteFromCell  -> _ -- print to $STDOUT
  ReadToCell     -> _ -- ask for input
  LoopStart      -> if cell == 0
                     then _ -- find the instruction AFTER the next LoopEnd
                     else tape
  LoopEnd        -> if cell /= 0
                     then tape
                     else _ -- find instruction right AFTER the last LoopStart
  where cell = Z.cursor tape

main = putStrLn "Hello World"
