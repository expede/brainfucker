module Main where

import qualified Data.List.Zipper    as Z
import qualified Data.Vector.Generic as V
import Data.Char (chr)

-- Tape for "memory"
newtype Cell = Int
newtype Tape = Zipper Cell

-- Machine for command sequence
newtype Index   = Int
newtype Machine = V.Vector Command

data Command = TapeLeft
             | TapeRight
             | IncrementCell
             | DecrementCell
             | PrintCell
             | OverwriteCell
             | LoopStart
             | LoopEnd
             | Ignore

charToCommand :: Char -> Command
charToCommand c = case c of
  '<' -> TapeLeft
  '>' -> TapeRight
  '+' -> IncrementCell
  '-' -> DecrementCell
  '.' -> PrintCell
  ',' -> OverwriteCell
  '[' -> LoopStart
  ']' -> LoopEnd
  _   -> Ignore

lex :: String -> Machine
lex = filter (/= Ignore) $ map charToCommand

loopStartIndex :: Index -> Machine -> Index
loopStartIndex startIndex machine = loopStartIndex' startIndex 0
  where loopStartIndex' index depth = case command of
          LoopStart -> if depth == 0
                        then index
                        else scanLeft $ pred depth
          LoopEnd   -> scanLeft $ succ depth
          _         -> scanLeft depth
          where scanLeft = loopStartIndex' (pred index)
                command  = machine ! index

loopEndIndex :: Index -> Machine -> Index
loopEndIndex startIndex machine = loopEndIndex' startIndex 0
  where loopEndIndex' index depth = case command of
          LoopEnd   -> if depth == 0
                        then index
                        else scanRight $ pred depth
          LoopStart -> scanRight $ succ depth
          _         -> scanRight depth
          where scanRight = loopEndIndex' (succ index)
                command   = machine ! index

main = do
  path    <- get
  machine <- lex $ readFile path
  tape    <- Z.Zip [] [0]
  stepMachine tape 0 machine

  where stepMachine tape index mach = case mach ! index of
    Ignore        -> step tape

    TapeLeft      -> step $ if beginp cell
                             then Z.insert 0 tape
                             else Z.left tape

    TapeRight     -> step $ if endp cell
                             then Z.insert 0 tape
                             else Z.right tape

    IncrementCell -> step $ Z.replace (succ cell) tape
    DecrementCell -> step $ Z.replace (pred cell) tape

    PrintCell     -> do putChar $ chr cell
                       step tape

    OverwriteCell -> do putStrLn "Enter a character: "
                       inChar  <- getChar
                       step $ Z.replace (ord inChar) tape

    LoopStart     -> if cell == 0 -- Loop is over; move past end
                      then stepMachine tape afterLoopEnd mach
                      else step tape

    LoopEnd       -> if cell == 0 -- Loop is over; move past end
                      then step tape
                      else stepMachine tape afterLoopStart mach

    where cell = Z.cursor tape
          step tape' = stepMachine tape' (succ index) mach
          afterLoopStart = succ loopEndIndex   index mach
          afterLoopEnd   = succ loopStartIndex index mach
