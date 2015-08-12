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
  , tapify
  , start
  , (###)
  , cursor
  , (#)
  , replaceCursor
  , adjustCursor
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

import Language.Brainfuck.Cell
import Test.QuickCheck (Gen, Arbitrary(..))
import qualified Data.List.Zipper as Z

-- | A collection of Cells is a Tape, which emulates "memory"
newtype Tape = Tape { unTape :: Z.Zipper Cell }

instance Show Tape where
  show tape = case unTape tape of
    Z.Zip as (c:bs) -> (unwords $ show <$> take 100 as)
                    ++ "  >>>[" ++ show c ++ "]<<< "
                    ++ (unwords $ show <$> take 100 bs)
    Z.Zip _ _ -> error "Finite Tape not possible"

instance Arbitrary Tape where
  arbitrary = toTape <$> (arbitrary :: Gen (Z.Zipper Cell))

toTape :: Z.Zipper Cell -> Tape
toTape (Z.Zip as bs) = Tape $ Z.Zip (fill as) (fill bs)
  where fill xs = xs ++ repeat (toCell 0)

tapify :: Z.Zipper Int -> Tape
tapify = toTape . fmap toCell

liftT :: (Z.Zipper Cell -> Z.Zipper Cell) -> Tape -> Tape
liftT func = toTape . func . unTape

-- $setup
-- >>> let tape0 = tapify $ Z.Zip [] [0]
-- >>> let tape1 = tapify $ Z.Zip [] [1]
-- >>> let tape9 = tapify $ Z.Zip [4,3,2,1,0] [5,6,7,8,9]
-- >>> let tapeX = tapify $ Z.Zip [] [88]

{- | Alternate syntax for `cursor`

>>> unCell $ cursor tape0
0

>>> unCell $ cursor tape1
1
-}
cursor :: Tape -> Cell
cursor = Z.cursor . unTape

{- | Alternate syntax for `cursor`

>>> unCell $ (#) tape0
0

>>> unCell $ (#) tape1
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
get = cellChr . (#)

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

>>> unCell . cursor $ set 'X' tape0
88

>>> unCell . cursor $ set '\343' tape0
88

>>> get $ set '\343' tape0
'X'
-}
set :: Char -> Tape -> Tape
set = replaceCursor . chrToCell

{- | Alterante syntax for `set`

>>> cursor $ (>#<) 'X' tape0
'X'

>>> cursor $ (>#<) '\343' tape0
'X'

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

>>> unCell $ cursor start
0

>>> unCell . cursor $ left start
0

>>> unCell . cursor $ right start
0
-}
start :: Tape
start = toTape Z.empty

{- | Alternate syntax for `start`

>>> unCell $ (#) (###)
0
-}
(###) :: Tape
(###) = start

{- | Move `Tape` cursor one position to the left

>>> unCell . cursor $ left tape9
4
-}
left :: Tape -> Tape
left = liftT Z.left

{- | Alternate syntax for `left`

>>> unCell . cursor $ (<<#) tape9
4
-}
(<<#) :: Tape -> Tape
(<<#) = left

{- | Move `Tape` cursor one position to the right

>>> unCell . cursor $ right tape9
6
-}
right :: Tape -> Tape
right = liftT Z.right

{- | Alternate syntax for `right`

>>> unCell . cursor $ (#>>) tape9
6
-}
(#>>) :: Tape -> Tape
(#>>) = liftT Z.right

{- Replace the `Tape`'s cursor with another `Cell`

>>> cursor $ replaceCursor (chrToCell 'X') tape0
'X'
-}
replaceCursor :: Cell -> Tape -> Tape
replaceCursor cell = liftT $ Z.replace cell

{- Adjust the existing `Tape`'s cursor

>>> cursor $ adjustCursor (* 88) tape1
'X'
-}
adjustCursor :: (Int -> Int) -> Tape -> Tape
adjustCursor func tape = replaceCursor (liftC func $ cursor tape) tape

{- | Increment the cell at the tape head, mod 255 (ASCII)

>>> unCell . cursor $ inc tape0
1

>>> unCell . cursor . inc . tapify $ Z.Zip [] [254]
0
-}
inc :: Tape -> Tape
inc = adjustCursor succ

{- | Alternate syntax for `inc`

>>> unCell . cursor $ (#++) tape0
1

>>> unCell . cursor . (#++) . tapify $ Z.Zip [] [254]
0
-}
(#++) :: Tape -> Tape
(#++) = inc

{- | Decrement the cell at the tape head, mod 256 (ASCII)

>>> unCell . cursor $ dec tape1
0

>>> unCell . cursor $ dec tape0
254
-}
dec :: Tape -> Tape
dec = adjustCursor pred

{- | Alternate syntax for `dec`

>>> unCell . cursor $ (#--) tape1
0

>>> unCell . cursor $ (#--) tape0
254
-}
(#--) :: Tape -> Tape
(#--) = dec
