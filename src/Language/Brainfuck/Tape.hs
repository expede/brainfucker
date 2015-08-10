{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Tape
Description : Tape (ie: memory) model, movement and operation on the tape head
-}

module Language.Brainfuck.Tape
  ( Cell
  , toCell
  , unCell
  , Tape(unTape)
  , toTape
  , start
  , (###)
  , cursor
  , (#)
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

import qualified Data.List.Zipper as Z
import Data.Char (chr, ord)
import Language.Brainfuck.Cell

-- | A collection of Cells is a Tape, which emulates "memory"
newtype Tape = Tape { unTape :: Z.Zipper Cell }
                 deriving (Show, Eq)

toTape :: Z.Zipper Cell -> Tape
toTape (Z.Zip as bs) = Tape $ Z.Zip (fill as) (fill bs)
  where fill xs = xs ++ repeat (toCell 0)

mapTape :: (Z.Zipper Cell -> Z.Zipper Cell) -> Tape -> Tape
mapTape func = toTape . func . unTape

-- $setup
-- >>> let tape0 = (Zip [] [0]) :: Tape
-- >>> let tape1 = (Zip [] [1]) :: Tape
-- >>> let tape9 = (Zip [4,3,2,1,0] [5,6,7,8,9]) :: Tape
-- >>> let tapeX = (Zip [] [88]) :: Tape

{- | Alternate syntax for `cursor`

>>> cursor tape0
0

>>> cursor tape1
1
-}
cursor :: Tape -> Cell
cursor = Z.cursor . unTape

{- | Alternate syntax for `cursor`

>>> (#) tape0
0

>>> (#) tape1
1
-}
(#) :: Tape -> Cell
(#) = cursor

{- | Get the ASCII letter at the tape's rw-head

>>> get tape0
'\NUL'

>>> get tape1
'\SOH'
-}
get :: Tape -> Char
get = chr . unCell . (#)

{- | Alternate syntax for `get`

>>> (<#>) tape0
'\NUL'

>>> (<#>) tape1
'\SOH'
-}
(<#>) :: Tape -> Char
(<#>) = get

{- | Replace the rw-head of the tape with another value.
Since this version of Brainfuck is ASCII, it mods at character value 128 ('\128')

>>> set 'X' tape0
Zip [] [88]

>>> set '\343' tape0
Zip [] [88]

>>> get $ set '\343' tape0
'X'
-}
set :: Char -> Tape -> Tape
set = replace . toCell . ord

{- | Alterante syntax for `set`

>>> (>#<) 'X' tape0
Zip [] [88]

>>> (>#<) '\343' tape0
Zip [] [88]

>>> (<#>) $ (>#<) '\343' tape0
'X'
-}
(>#<) :: Char -> Tape -> Tape
(>#<) = set

{-| Print the tape's head

>>> printCell $ tapeX
X
-}
printCell :: Tape -> IO ()
printCell = putChar . get

{- | Alternate syntax for `printCell`

>>> (^#^) $ tapeX
X
-}
(^#^) :: Tape -> IO ()
(^#^) = printCell

{- | Starting position of an infinite tape of 0s

>>> cursor start
0

>>> cursor $ left start
0

>>> cursor $ right start
0
-}
start :: Tape
start = toTape Z.empty

{- | Alternate syntax for `start`

>>> (#) (###)
0
-}
(###) :: Tape
(###) = start

{- | Move `Tape` cursor one position to the left

>>> left tape9
Zip [3,2,1,0] [4,5,6,7,8,9]
-}
left :: Tape -> Tape
left = mapTape Z.left

{- | Alternate syntax for `left`

>>> (<<#) tape9
Zip [3,2,1,0] [4,5,6,7,8,9]
-}
(<<#) :: Tape -> Tape
(<<#) = left

{- | Move `Tape` cursor one position to the right

>>> right tape9
Zip [5,4,3,2,1,0] [6,7,8,9]
-}
right :: Tape -> Tape
right = mapTape Z.right

{- | Alternate syntax for `right`

>>> (#>>) tape9
Zip [5,4,3,2,1,0] [6,7,8,9]
-}
(#>>) :: Tape -> Tape
(#>>) = mapTape Z.right

replace :: Cell -> Tape -> Tape
replace cell tape = toTape $ Z.replace cell (unTape tape)

{- | Increment the cell at the tape head, mod 255 (ASCII)

>>> inc tape0
Zip [] [1]

>>> inc $ Zip [] [254]
Zip [] [0]
-}
inc :: Tape -> Tape
inc tape = replace (succ `mapCell` cursor tape) tape

{- | Alternate syntax for `inc`

>>> (#++) tape0
Zip [] [1]

>>> (#++) $ Zip [] [254]
Zip [] [0]
-}
(#++) :: Tape -> Tape
(#++) = inc

{- | Decrement the cell at the tape head, mod 256 (ASCII)

>>> dec tape1
Zip [] [0]

>>> dec tape0
Zip [] [254]
-}
dec :: Tape -> Tape
dec tape = replace (pred `mapCell` cursor tape) tape

{- | Alternate syntax for `dec`

>>> (#--) tape1
Zip [] [0]

>>> (#--) tape0
Zip [] [254]
-}
(#--) :: Tape -> Tape
(#--) = dec
