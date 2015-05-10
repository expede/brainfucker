{-# LANGUAGE OverloadedStrings #-}

module Data.Brainfucker.Lex (toAST) where
import Data.Brainfucker.AST
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Attoparsec.Internal.Types (Parser)
import Data.Attoparsec.Text ( char
                            , anyChar
                            , many1
                            , parseOnly
                            )

type Lexer = Parser Text (AST ())

(==>) :: Char -> AST () -> Lexer
c ==> lexeme = char c *> return lexeme

isTapeL :: Lexer
isTapeL = '<' ==> tapeL

isTapeR :: Lexer
isTapeR = '>' ==> tapeR

isIncCell :: Lexer
isIncCell = '+' ==> incCell

isDecCell :: Lexer
isDecCell = '-' ==> decCell

isGetCell :: Lexer
isGetCell = '.' ==> getCell

isSetCell :: Lexer
isSetCell = ',' ==> setCell

inLoop :: Lexer
inLoop = do
   char '['
   nodes <- many1 node
   char ']'
   return . subtree $ sequence_ nodes

ignore :: Lexer
ignore = anyChar *> node

node :: Lexer
node =  isTapeL
    <|> isTapeR
    <|> isIncCell
    <|> isDecCell
    <|> isSetCell
    <|> isGetCell
    <|> inLoop
    <|> ignore

toAST :: Text -> AST ()
toAST text = case parsed of
  Right result -> sequence_ result >> end
  Left  err    -> error err
  where parsed = parseOnly (many1 node) text
