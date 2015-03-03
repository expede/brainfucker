module Main where

import qualified Data.List.Zipper    as Z
import qualified Data.Vector.Generic as V

-- Tape for "memory"
newtype Cell    = Int
newtype Tape    = Zipper Cell

-- Machine for command sequence
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
  _   -> Ignore -- Optimize out later by skipping

lex :: String -> Machine
lex = map charToCommand

afterLoopStart :: Index -> Machine -> Index
afterLoopStart ind machine = if machine ! ind == LoopStart
                               then succ ind
                               else findLoopStart (pred ind) machine

afterLoopEnd :: Index -> Machine -> Index
afterLoopEnd ind machine = if machine ! (pred ind) == LoopEnd
                             then succ ind
                             else indexAfterLoop (succ ind) machine

-- This probably belongs in `main`, for obvious reasons
stepMachine :: Tape -> Index -> Machine -> (Maybe IO (), Tape)
stepMachine (Z.Zip _ []) ind mach = step $ Z.insert 0 tape
stepMachine tape         ind mach = case mach ! ind of
  TapeLeft      -> step $ Z.left tape
  TapeRight     -> step $ Z.right tape
  IncrementCell -> step $ Z.replace (succ cell) tape
  DecrementCell -> step $ Z.replace (pred cell) tape
  WriteFromCell -> _ -- $STDOUT cell and next
  ReadToCell    -> _ -- ask for input; `next $ Z.replace input tape`
  LoopStart     -> if cell == 0
                    then stepMachine tape (afterLoopStart ind mach)
                    else step tape
  LoopEnd       -> if cell /= 0
                    then stepMachine tape (afterLoopEnd ind mach)
                    else step tape
  _             -> next tape
  where cell = Z.cursor tape
        step tape' = stepMachine tape' (succ ind) machfd fs

main = putStrLn "Hello World"
