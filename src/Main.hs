module Main where

import qualified Data.List.Zipper as Z
import qualified Data.Vector      as V
import qualified Data.Char        as C

-- Tape for "memory"
type Cell = Int
type Tape = Z.Zipper Cell

-- Machine for command sequence
type Index   = Int
type Machine = V.Vector Command

data Command = TapeLeft
             | TapeRight
             | IncrementCell
             | DecrementCell
             | PrintCell
             | OverwriteCell
             | LoopStart
             | LoopEnd
             | Ignore
             deriving (Show, Eq)

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

toMachine :: String -> Machine
toMachine = V.fromList . (filter (/= Ignore)) . (map charToCommand)

loopStartIndex :: Index -> Machine -> Index
loopStartIndex startIndex machine = loopStartIndex' startIndex 0
  where loopStartIndex' index depth = case command of
          LoopStart -> if depth == 0
                        then index
                        else scanLeft $ pred depth
          LoopEnd   -> scanLeft $ succ depth
          _         -> scanLeft depth
          where scanLeft = loopStartIndex' (pred index)
                command  = machine V.! index

loopEndIndex :: Index -> Machine -> Index
loopEndIndex startIndex machine = loopEndIndex' startIndex 0
  where loopEndIndex' index depth = case command of
          LoopEnd   -> if depth == 0
                        then index
                        else scanRight $ pred depth
          LoopStart -> scanRight $ succ depth
          _         -> scanRight depth
          where scanRight = loopEndIndex' (succ index)
                command   = machine V.! index

main = do
  path    <- getLine
  file    <- readFile path
  machine <- return $ toMachine file
  tape    <- return $ Z.Zip [] [0]
  return $ stepMachine tape 0 machine

  where
    stepMachine tape index mach = case mach V.! index of
      Ignore        -> step tape

      TapeLeft      -> step $ if Z.beginp tape
                               then Z.insert 0 tape
                               else Z.left tape

      TapeRight     -> step $ if Z.endp tape
                               then Z.insert 0 tape
                               else Z.right tape

      IncrementCell -> step $ Z.replace (succ cell) tape
      DecrementCell -> step $ Z.replace (pred cell) tape

      PrintCell     -> do
                        putChar (C.chr cell)
                        step tape

      OverwriteCell -> do
                        putStrLn "Enter a character: "
                        inChar <- getChar
                        step $ Z.replace (C.ord inChar) tape

      LoopStart     -> if cell == 0 -- Loop is over; move past end
                        then stepMachine tape afterLoopEnd mach
                        else step tape

      LoopEnd       -> if cell == 0 -- Loop is over; move past end
                        then step tape
                        else stepMachine tape afterLoopStart mach

      where cell = Z.cursor tape :: Int
            step tape' = stepMachine tape' (succ index) mach
            afterLoopStart = succ $ loopEndIndex   index mach
            afterLoopEnd   = succ $ loopStartIndex index mach
