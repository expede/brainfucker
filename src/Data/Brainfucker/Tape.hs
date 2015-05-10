-- Tape (ie. "memory") model
module Data.Brainfucker.Tape
  ( Cell
  , Tape
  , start
  , (###)
  , cursor
  , (#)
  , isCursorZero
  , (#?)
  , get
  , (<#>)
  , printCell
  , (^#^)
  , set
  , (>#<)
  , left
  , (<<#)
  , right
  , (#>>)
  , inc
  , (#++)
  , dec
  , (#--)
  ) where

import Data.Char (ord, chr)
import qualified Data.List.Zipper as Z

type Cell = Int
type Tape = Z.Zipper Cell

cursor :: Tape -> Cell
cursor = Z.cursor

(#) :: Tape -> Cell
(#) = cursor

isCursorZero :: Tape -> Bool
isCursorZero = (== 0) . cursor

(#?) :: Tape -> Bool
(#?) = isCursorZero

get :: Tape -> Char
get = chr . cursor

(<#>) :: Tape -> Char
(<#>) = get

printCell :: Tape -> IO ()
printCell = putChar . get

(^#^) :: Tape -> IO ()
(^#^) = printCell

set :: Tape -> Char -> Tape
set tape char = Z.replace newInt tape
  where newInt = ord char `mod` 128

(>#<) :: Tape -> Char -> Tape
(>#<) = set

-- | Starting position of a Tape.
-- Has one '0' to the left of the cursor
-- This will get filled in on first run of runMachine, anyway
--
-- >>> tapeStart
-- Zip [0] [0]
start :: Tape
start = Z.Zip [0] [0]

(###) :: Tape
(###) = start

left :: Tape -> Tape
left (Z.Zip [] d) = (Z.Zip [] (0:d))
left tape         = Z.left tape

(<<#) :: Tape -> Tape
(<<#) = left

right :: Tape -> Tape
right (Z.Zip b (c:[])) = (Z.Zip (c:b) [0])
right tape             = Z.right tape

(#>>) :: Tape -> Tape
(#>>) = right

inc :: Tape -> Tape
inc   (Z.Zip _ []   ) = error "not possible"
inc t@(Z.Zip _ (c:_)) = Z.replace (succ c `mod` 128) t

(#++) :: Tape -> Tape
(#++) = inc

dec :: Tape -> Tape
dec   (Z.Zip _ []   ) = error "not possible"
dec t@(Z.Zip _ (c:_)) = Z.replace (pred c `mod` 128) t

(#--) :: Tape -> Tape
(#--) = dec
