{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}

{-|
Module      : Tape
Description : Tape (ie: memory) model, movement and operation on the tape head
-}

module Language.Brainfuck.Tape
  ( Cell
  , Tape
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

import Data.Char (ord, chr)
import qualified Data.List.Zipper as Z

-- | Cell is a single memory cell in the Tape
type Cell = Int

-- | A collection of Cells is a Tape, which emulates "memory"
type Tape = Z.Zipper Cell

-- $setup
-- >>> let tape0 = (Z.Zip [] [0]) :: Tape
-- >>> let tape1 = (Z.Zip [] [1]) :: Tape
-- >>> let tape9 = (Z.Zip [4,3,2,1,0] [5,6,7,8,9]) :: Tape
-- >>> let tapeX = (Z.Zip [] [88]) :: Tape

{- | Get the current tape's head value

>>> cursor start
0

>>> cursor tape1
1
-}
cursor :: Tape -> Cell
cursor = Z.cursor

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
get = chr . cursor

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

>>> set tape0 'X'
Zip [] [88]

>>> set tape0 '\216'
Zip [] [88]

>>> get $ set tape0 '\216'
'X'
-}
set :: Tape -> Char -> Tape
set tape char = Z.replace newInt tape
  where newInt = ord char `mod` 128

{- | Alterante syntax for `set`

>>> (>#<) tape0 'X'
Zip [] [88]

>>> (>#<) tape0 '\216'
Zip [] [88]

>>> (<#>) $ (>#<) tape0 '\216'
'X'
-}
(>#<) :: Tape -> Char -> Tape
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
start = Z.Zip (repeat 0) (repeat 0)

{- | Alternate syntax for `start`

>>> (#) (###)
0
-}
(###) :: Tape
(###) = start

{- | Move tape head left one position

>>> left tape9
Zip [3,2,1,0] [4,5,6,7,8,9]
-}
left :: Tape -> Tape
left = Z.left

{- | Alternate syntax for `left`

>>> (<<#) tape9
Zip [3,2,1,0] [4,5,6,7,8,9]
-}
(<<#) :: Tape -> Tape
(<<#) = left

{- | Move tape head right one position

>>> right tape9
Zip [5,4,3,2,1,0] [6,7,8,9]
-}
right :: Tape -> Tape
right = Z.right

{- | Alternate syntax for `right`

>>> (#>>) tape9
Zip [5,4,3,2,1,0] [6,7,8,9]
-}
(#>>) :: Tape -> Tape
(#>>) = right

{- | Increment the cell at the tape head, mod 128 (ASCII)

>>> inc tape0
Zip [] [1]

>>> inc $ Z.Zip [] [127]
Zip [] [0]
-}
inc :: Tape -> Tape
inc   (Z.Zip _ []   ) = error "not possible"
inc t@(Z.Zip _ (c:_)) = Z.replace (succ c `mod` 128) t

{- | Alternate syntax for `inc`

>>> (#++) tape0
Zip [] [1]

>>> (#++) $ Z.Zip [] [127]
Zip [] [0]
-}
(#++) :: Tape -> Tape
(#++) = inc

{- | Decrement the cell at the tape head, mod 128 (ASCII)

>>> dec tape1
Zip [] [0]

>>> dec tape0
Zip [] [127]
-}
dec :: Tape -> Tape
dec   (Z.Zip _ []   ) = error "not possible"
dec t@(Z.Zip _ (c:_)) = Z.replace (pred c `mod` 128) t

{- | Alternate syntax for `dec`

>>> (#--) tape1
Zip [] [0]

>>> (#--) tape0
Zip [] [127]
-}
(#--) :: Tape -> Tape
(#--) = dec
