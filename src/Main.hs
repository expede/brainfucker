module Main where

import qualified Data.List.Zipper    as Z
import qualified Data.Vector.Generic as V
import Data.Char (chr)

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
             | PrintCell
             | OverwriteCell
             | LoopStart
             | LoopEnd

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
loopStartIndex ind machine = if machine ! ind == LoopStart
                               then ind
                               else loopStartIndex (pred ind) machine

loopEndIndex :: Index -> Machine -> Index
loopEndIndex ind machine = if machine ! ind == LoopEnd
                             then ind
                             else loopEndIndex (succ ind) machine

main = do
  path    <- get
  machine <- readFile path
  tape    <- Z.Zip [] [0]
  stepMachine tape 0 machine

  where stepMachine tape ind mach = case mach ! ind of
    TapeLeft      -> step $ if beginp cell
                             then Z.insert 0 tape
                             else Z.left tape

    TapeRight     -> step $ if endp cell
                             then Z.insert 0 tape
                             else Z.right tape

    IncrementCell -> step $ Z.replace (succ cell) tape
    DecrementCell -> step $ Z.replace (pred cell) tape

    PrintCell     -> do putChar $ chr cell
                       stepMachine tape nextInd mach

    OverwriteCell -> do putStrLn "Enter a character: "
                       inChar  <- getChar
                       newTape <- Z.replace (ord inChar) tape
                       stepMachine newTape nextInd mach

    LoopStart     -> if cell == 0 -- Loop over; move past end
                      then stepMachine tape afterLoopEnd mach
                      else step tape

    LoopEnd       -> if cell == 0 -- Loop over; move past end
                      then step tape
                      else stepMachine tape afterLoopStart mach

    where cell = Z.cursor tape
          nextInd = succ ind
          step tape' = stepMachine tape' nextInd mach
          afterLoopStart = succ loopEndIndex ind mach
          afterLoopEnd   = succ loopStartIndex ind mach
