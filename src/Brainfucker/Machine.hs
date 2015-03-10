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

type Count = Int
data Command = Command Lexeme Count
             | Loop Machine
               deriving (Show)

-- | Convert characters to lexemes
-- Flag any non-brainfuck characters for Ignoring
--
-- Examples:
--
-- >>> lexChar ','
-- OverwriteCell
--
-- >>> map lexChar "abc[+]#"
-- [Ignore,Ignore,Ignore,LoopStart,IncrementCell,LoopEnd,Ignore]
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

-- | Convert string input to a Machine AST
-- Repeated commands tracked with an Int for efficiency later
-- ',' and '.' are never collapsed (always return a Count of 1)
--
-- Examples:
--
-- >>> toMachine "abc[+]#"
-- [Loop [Command IncrementCell 1]]
--
-- >>> toMachine "++."
-- [Command IncrementCell 2,Command PrintCell 1]
--
-- >>> toMachine "..."
-- [Command PrintCell 1,Command PrintCell 1,Command PrintCell 1]
toMachine :: String -> Machine
toMachine = toMachine' . map lexChar
  where toMachine' = commFold []
        commFold acc []     = acc
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
