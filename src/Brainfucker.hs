module Brainfucker
       ( interpret
       , runMachine
       , module Machine
       , module Tape
       ) where

import           Brainfucker.Machine as Machine
import           Brainfucker.Tape    as Tape

import qualified Data.Char           as C (chr, ord)
import qualified Data.List.Zipper    as Z ( Zipper(..)
                                          , cursor
                                          , left
                                          , right
                                          , replace
                                          )

-- | Convenience function for calling `toMachine` on a string
--
-- Examples:
--
-- >>> interpret "+++++++++++++++++++++++++++++++++."
-- !
interpret :: String -> IO ()
interpret = runMachine tapeStart . toMachine

-- | Explicit recursive function for interpreting `Machine`s
--
-- TODO: convert to a free monad, once the `Machine` type is fixed
--
-- Examples:
--
-- >>> runMachine tapeStart [Command IncrementCell 50,Command PrintCell 1]
-- 2
runMachine :: Z.Zipper Cell -> Machine -> IO ()
runMachine _             []                 = error "No program input" -- WOW, THIS LINE IS BROKEN!
runMachine (Z.Zip [] []) m                  = runMachine tapeStart m
runMachine (Z.Zip [] t)  m                  = runMachine (Z.Zip [0] t  ) m
runMachine (Z.Zip h  []) m                  = runMachine (Z.Zip h   [0]) m
runMachine t             (Loop m : _)       = runMachine t m
runMachine t             (Command l n : cs) =
  case l of
    TapeLeft      -> step $ timesDo Z.left
    TapeRight     -> step $ timesDo Z.right
    IncrementCell -> step $ updateCell (+ n)
    DecrementCell -> step $ updateCell (subtract n)
    PrintCell     -> do
                       putChar $ C.chr cell
                       step t
    OverwriteCell -> do
                       putStrLn "Enter a character: "
                       inChar <- getChar
                       step $ Z.replace (C.ord inChar) t
    _             -> step t
  where cell    = Z.cursor t :: Int
        step t' = runMachine t' cs
        timesDo f    = (!! n) . iterate f $ t
        updateCell f = Z.replace (f cell `mod` 255) t
