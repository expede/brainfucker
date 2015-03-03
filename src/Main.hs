module Main where

import Data.List.Zipper

-- Using a "tape" metaphor
newtype Tape = [Int]

data Command = MoveLeft
             | MoveRight
             | IncrementValue
             | DecrementValue
             | WriteOut
             | ReadIn
             | JumpForward
             | JumpBackward

charToCommand :: Char -> Command
charToCommand c = case c of
  '<' -> MoveLeft
  '>' -> MoveRight
  '+' -> IncrementValue
  '-' -> DecrementValue
  '.' -> WriteOut
  ',' -> ReadIn
  '[' -> JumpForward
  ']' -> JumpBackward
  _   -> error "invalid character"

lex :: String -> [Command]
lex = map charToCommand

main = putStrLn "Hello World"
