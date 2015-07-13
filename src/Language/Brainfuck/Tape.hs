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

import Data.List.Zipper ( Zipper(..)
                        , cursor
                        , replace
                        , left
                        , right
                        )

import Data.Char ( chr
                 , ord
                 )

-- | Cell is a single memory cell in the Tape
type Cell = Int

-- | A collection of Cells is a Tape, which emulates "memory"
type Tape = Zipper Cell

-- $setup
-- >>> let tape0 = (Zip [] [0]) :: Tape
-- >>> let tape1 = (Zip [] [1]) :: Tape
-- >>> let tape9 = (Zip [4,3,2,1,0] [5,6,7,8,9]) :: Tape
-- >>> let tapeX = (Zip [] [88]) :: Tape

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

>>> set 'X' tape0
Zip [] [88]

>>> set '\216' tape0
Zip [] [88]

>>> get $ set '\216' tape0
'X'
-}
set :: Char -> Tape -> Tape
set char tape = replace newInt tape
  where newInt = ord char `mod` 128

{- | Alterante syntax for `set`

>>> (>#<) 'X' tape0
Zip [] [88]

>>> (>#<) '\216' tape0
Zip [] [88]

>>> (<#>) $ (>#<) '\216' tape0
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
start = Zip (repeat 0) (repeat 0)

{- | Alternate syntax for `start`

>>> (#) (###)
0
-}
(###) :: Tape
(###) = start

{- | Alternate syntax for `left`

>>> (<<#) tape9
Zip [3,2,1,0] [4,5,6,7,8,9]
-}
(<<#) :: Tape -> Tape
(<<#) = left

{- | Alternate syntax for `right`

>>> (#>>) tape9
Zip [5,4,3,2,1,0] [6,7,8,9]
-}
(#>>) :: Tape -> Tape
(#>>) = right

{- | Increment the cell at the tape head, mod 128 (ASCII)

>>> inc tape0
Zip [] [1]

>>> inc $ Zip [] [127]
Zip [] [0]
-}
inc :: Tape -> Tape
inc   (Zip _ []   ) = error "not possible"
inc t@(Zip _ (c:_)) = replace (succ c `mod` 128) t

{- | Alternate syntax for `inc`

>>> (#++) tape0
Zip [] [1]

>>> (#++) $ Zip [] [127]
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
dec   (Zip _ []   ) = error "not possible"
dec t@(Zip _ (c:_)) = replace (pred c `mod` 128) t

{- | Alternate syntax for `dec`

>>> (#--) tape1
Zip [] [0]

>>> (#--) tape0
Zip [] [127]
-}
(#--) :: Tape -> Tape
(#--) = dec
