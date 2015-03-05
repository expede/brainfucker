-- The command sequence that operates on the Tape
module Brainfucker.Machine
       ( Command(..)
       , Lexeme(..)
       , Machine
       , toMachine
       ) where

type Machine = [Command]

data Lexeme = TapeLeft
            | TapeRight
            | IncrementCell
            | DecrementCell
            | PrintCell
            | OverwriteCell
            | LoopStart
            | LoopEnd
            | Ignore
             deriving (Eq, Show)

data Command = Command Lexeme Int
             | Loop Machine

lexChar :: Char -> Lexeme
lexChar c = case c of
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
toMachine = toMachine' . map lexChar
  where toMachine' = commFold []
        commFold acc (x:xs) = case x of
          LoopStart -> acc ++ [Loop $ toMachine' xs]
          LoopEnd   -> acc
          PrintCell     -> acc ++ [Command PrintCell     1]
          OverwriteCell -> acc ++ [Command OverwriteCell 1]
          Ignore    -> commFold acc xs
          _         -> commFold (acc ++ [Command x headsLength]) commandsTail
          where spanHead     = span (== x) (x:xs)
                commandsTail = snd spanHead
                headsLength  = length $ fst spanHead
