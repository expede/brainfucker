{-|
Module      : Brainfuck
Description : Interpret a Brainfuck program
-}

module Language.Brainfuck ( Tape
                          , AST
                          , Bfk(..)
                          , interpret
                          , interpret'
                          , toAST
                          , start
                          , (###)
                          ) where

import Language.Brainfuck.Lex (toAST)

import Language.Brainfuck.Tape ( Tape
                               , start
                               , (###)
                               )

import Language.Brainfuck.AST ( AST
                              , Bfk(..)
                              )

import Language.Brainfuck.Interpret ( interpret
                                    , interpret'
                                    )
