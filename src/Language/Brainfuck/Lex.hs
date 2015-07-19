{-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Lex
Description : Convert text to brainfuck AST
-}

module Language.Brainfuck.Lex ( Lexer
                              , (==>)
                              , isTapeL
                              , isTapeR
                              , isIncCell
                              , isDecCell
                              , isGetCell
                              , isSetCell
                              , inLoop
                              , ignore
                              , node
                              , toAST
                              ) where

import Language.Brainfuck.AST
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Attoparsec.Internal.Types (Parser)
import Data.Attoparsec.Text ( char
                            , anyChar
                            , manyTill'
                            , many'
                            , parseOnly
                            , skip
                            )

-- | Lexer is the expected parser type for the Brainfuck AST
type Lexer = Parser Text (AST ())

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let badText = "wrong" :: Text

{-| Equate a character with a lexeme

>>> parseOnly ('+' ==> incCell) ("+" :: Text)
Right (Free (IncCell (Pure ())))

>>> parseOnly ('+' ==> incCell) badText
Left "'+': Failed reading: satisfy"
-}
(==>) :: Char -> AST () -> Lexer
c ==> lexeme = char c *> return lexeme

{- | Lex '<' as `tapeL`

>>> parseOnly isTapeL ("<" :: Text)
Right (Free (TapeL (Pure ())))

>>> parseOnly isTapeL badText
Left "'<': Failed reading: satisfy"
-}
isTapeL :: Lexer
isTapeL = '<' ==> tapeL

{- | Lex '>' as `tapeR`

>>> parseOnly isTapeR (">" :: Text)
Right (Free (TapeR (Pure ())))

>>> parseOnly isTapeR badText
Left "'>': Failed reading: satisfy"
-}
isTapeR :: Lexer
isTapeR = '>' ==> tapeR

{- | Lex '+' as `incCell`

>>> parseOnly isIncCell ("+" :: Text)
Right (Free (IncCell (Pure ())))

>>> parseOnly isIncCell badText
Left "'+': Failed reading: satisfy"
-}
isIncCell :: Lexer
isIncCell = '+' ==> incCell

{- | Lex '-' as `decCell`

>>> parseOnly isDecCell ("-" :: Text)
Right (Free (DecCell (Pure ())))

>>> parseOnly isDecCell badText
Left "'-': Failed reading: satisfy"
-}
isDecCell :: Lexer
isDecCell = '-' ==> decCell

{- | Lex '.' as `getCell`

>>> parseOnly isGetCell ("." :: Text)
Right (Free (GetCell (Pure ())))

>>> parseOnly isGetCell badText
Left "'.': Failed reading: satisfy"
-}
isGetCell :: Lexer
isGetCell = '.' ==> getCell

{- | Lex ',' as `setCell`

>>> parseOnly isSetCell ("," :: Text)
Right (Free (SetCell (Pure ())))

>>> parseOnly isSetCell badText
Left "',': Failed reading: satisfy"
-}
isSetCell :: Lexer
isSetCell = ',' ==> setCell

{- | Recursively lex "[x]" as `Loop (x) ()`

>>> parseOnly inLoop ("[]" :: Text)
Right (Free (Loop (Free End) (Pure ())))

>>> parseOnly inLoop ("[.]" :: Text)
Right (Free (Loop (Free (GetCell (Free End))) (Pure ())))

>>> parseOnly inLoop ("[[.]]" :: Text)
Right (Free (Loop (Free (Loop (Free (GetCell (Free End))) (Free End))) (Pure ())))

>>> parseOnly inLoop ("[.[.]]" :: Text)
Right (Free (Loop (Free (GetCell (Free (Loop (Free (GetCell (Free End))) (Free End))))) (Pure ())))

>>> parseOnly inLoop ("[[.].]" :: Text)
Right (Free (Loop (Free (Loop (Free (GetCell (Free End))) (Free (GetCell (Free End))))) (Pure ())))

>>> parseOnly inLoop ("[.[+,[-]]]" :: Text)
Right (Free (Loop (Free (GetCell (Free (Loop (Free (IncCell (Free (SetCell (Free (Loop (Free (DecCell (Free End))) (Free End))))))) (Free End))))) (Pure ())))

>>> parseOnly inLoop ("[+[-?]]" :: Text)
Right (Free (Loop (Free (IncCell (Free (Loop (Free (DecCell (Free End))) (Free End))))) (Pure ())))

>>> parseOnly inLoop ("[+[-].]" :: Text)
Right (Free (Loop (Free (IncCell (Free (Loop (Free (DecCell (Free End))) (Free (GetCell (Free End))))))) (Pure ())))

>>> parseOnly isSetCell badText
Left "',': Failed reading: satisfy"
-}
inLoop :: Lexer
inLoop = char '[' *> node `manyTill'` char ']' >>= \x -> return $ subtree $ sequence_ x-- do

{- | Skip past all non-brainfuck characters.
Delegates first brainfuck match to the correct parser.
If no match, then skip and move to the next character.

>>> parseOnly ignore badText
Right (Pure ())

>>> parseOnly ignore ("++" :: Text)
Right (Pure ())

>>> parseOnly ignore ("?__+,__???07821900??+??8980____?????" :: Text)
Right (Pure ())
-}
ignore :: Lexer
ignore = anyChar *> return suspend

{- | First-match lexing tree.
Tries all possibilities before loops or ignored characters.

>>> parseOnly node badText
Right (Pure ())

>>> parseOnly ignore ("++" :: Text)
Right (Pure ())

>>> parseOnly ignore ("?__+,__???07821900??+??8980____?????" :: Text)
Right (Pure ())
-}
node :: Lexer
node =  isTapeL
    <|> isTapeR
    <|> isIncCell
    <|> isDecCell
    <|> isSetCell
    <|> isGetCell
    <|> inLoop
    <|> ignore

{- | Repeatedly run `node` over the input text, and join them into an `AST`.
Throws a runtime error with message if the input text is malfomed or unparseable.

>>> toAST ("+-" :: Text)
Free (IncCell (Free (DecCell (Pure ()))))

>>> toAST ("[[+]]" :: Text)
Free (Loop (Free (Loop (Free (IncCell (Free End))) (Free End))) (Pure ()))

>>> toAST ("[[+]-]" :: Text)
Free (Loop (Free (Loop (Free (IncCell (Free End))) (Free (DecCell (Free End))))) (Pure ()))

>>> toAST (",[[+]-]" :: Text)
Free (SetCell (Free (Loop (Free (Loop (Free (IncCell (Free End))) (Free (DecCell (Free End))))) (Pure ()))))

>>> toAST badText
Pure ()
-}
toAST :: Text -> AST ()
toAST text = case parsed of
  Right result -> sequence_ result
  Left  err    -> error err
  where parsed = parseOnly (many' node) text
