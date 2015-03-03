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
             | ControlJumpRight
             | ControlJumpLeft

charToCommand :: Char -> Command
charToCommand c = case c of
  '<' -> TapeLeft
  '>' -> TapeRight
  '+' -> IncrementCell
  '-' -> DecrementCell
  '.' -> WriteFromCell
  ',' -> ReadToCell
  '[' -> ControlJumpRight
  ']' -> ControlJumpLeft
  _   -> error "invalid character"

lex :: String -> Machine
lex = map charToCommand

operateMachine :: Tape -> Command -> Tape
operateMachine (Zip [] []) _       = Z.insert 0 tape
operateMachine tape        command = case command of
  TapeLeft         -> Z.left tape
  TapeRight        -> Z.right tape
  IncrementCell    -> Z.replace (succ pointer) tape
  DecrementCell    -> Z.replace (pred pointer) tape
  WriteFromCell    -> _ -- print to $STDOUT
  ReadToCell       -> _ -- ask for input
  ControlJumpRight -> if pointer == 0
                       then _ -- find the instruction AFTER the next JumpBackward
                       else tape
  ControlJumpLeft  -> if pointer /= 0
                       then tape
                       else _ -- find instruction right AFTER the last JumpForward
  where pointer = Z.cursor tape

main = putStrLn "Hello World"
