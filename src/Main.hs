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
             deriving (Eq, Show)

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
          LoopStart -> case depth of
                        1 -> index
                        _ -> scanLeft $ pred depth
          LoopEnd   -> scanLeft $ succ depth
          _         -> scanLeft depth

          where scanLeft = loopStartIndex' (pred index)
                command  = machine V.! index

loopEndIndex :: Index -> Machine -> Index
loopEndIndex startIndex machine = loopEndIndex' startIndex 0
  where loopEndIndex' index depth = case command of
          LoopEnd   -> case depth of
                        1 -> index
                        _ -> scanRight $ pred depth
          LoopStart -> scanRight $ succ depth
          _         -> scanRight depth

          where scanRight = loopEndIndex' (succ index)
                command   = machine V.! index

main = do
  putStrLn "Path to input brainfuck file: "
  path    <- getLine
  file    <- readFile path
  machine <- return $ toMachine file
  tape'   <- return $ Z.Zip [] [0]
  stepMachine tape' 0 machine

  where
    stepMachine tape index mach = runOrEnd $ case mach V.! index of
      Ignore        -> step tape

      TapeLeft      -> step $ if Z.beginp tape
                               then Z.insert 0 tape
                               else Z.left tape

      TapeRight     -> step $ if Z.endp tape
                               then Z.insert 0 tape
                               else Z.insert 0 (Z.right tape)

      IncrementCell -> step $ case cell of
                        255 -> Z.replace 0 tape
                        _   -> Z.replace (succ cell) tape

      DecrementCell -> step $ case cell  of
                        0 -> Z.replace 255 tape
                        _ -> Z.replace (pred cell) tape

      PrintCell     -> do
                         putStrLn $ show tape
                         putChar $ C.chr cell
                         step tape

      OverwriteCell -> do
                         putStrLn "Enter a character: "
                         inChar <- getChar
                         step $ Z.replace (C.ord inChar) tape

      LoopStart     -> case cell of
                        0 -> stepMachine tape afterLoopEnd mach
                        _ -> step tape

      LoopEnd       -> case cell of
                        0 -> step tape
                        _ -> stepMachine tape afterLoopStart mach

      where cell = Z.cursor tape :: Int
            step tape''    = stepMachine tape'' (succ index) mach
            afterLoopStart = succ $ loopStartIndex index mach
            afterLoopEnd   = succ $ loopEndIndex   index mach
            runOrEnd func  = if index < V.length mach
                               then func
                               else putStrLn "0"
