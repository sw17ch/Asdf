
module Language.Asdf.Lexer  ( symbol
                            , parens
                            , brackets
                            , natural
                            , naturalOrFloat
                            , rational
                            , stringLiteral
                            , operator
                            , identifier
                            , reserved
                            , reservedOp
                            , whiteSpace
                            , comma
                            , lexer
                            ) where

import Control.Monad

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

lexerStyle :: (Monad m) => GenLanguageDef String u m
lexerStyle = Token.LanguageDef
                { Token.commentStart   = "{-"
                , Token.commentEnd     = "-}"
                , Token.commentLine    = "//"
                , Token.nestedComments = True
                , Token.identStart     = letter
                , Token.identLetter    = alphaNum <|> oneOf "_'#" 
                , Token.opStart        = Token.opLetter lexerStyle
                , Token.opLetter       = oneOf "~!@$%^&*-+/?|=<>" 
                , Token.reservedOpNames= ["::"]
                , Token.reservedNames  = [ "return", "struct", "union"
                                         , "for", "while", "if", "else"
                                         , "do", "var", "foreign", "def"]
                , Token.caseSensitive  = True
                }

lexer :: (Monad m) => Token.GenTokenParser String u m
lexer = Token.makeTokenParser lexerStyle

symbol :: (Monad m) => String -> ParsecT String u m String
symbol = Token.symbol lexer

parens, brackets :: (Monad m) => ParsecT String u m a -> ParsecT String u m a
parens = Token.parens lexer
brackets = Token.brackets lexer

natural :: (Monad m) => ParsecT String u m Integer
natural = Token.natural lexer

naturalOrFloat :: (Monad m) => ParsecT String u m (Either Integer Double)
naturalOrFloat = Token.naturalOrFloat lexer

rational :: (Monad m) => ParsecT String u m Rational
rational = liftM (either toRational toRational) naturalOrFloat

stringLiteral, identifier, operator, comma :: (Monad m) => ParsecT String u m String
stringLiteral = Token.stringLiteral lexer

operator = Token.operator lexer

identifier = Token.identifier lexer

comma = Token.comma lexer

reservedOp :: (Monad m) => String -> ParsecT String u m ()
reservedOp = Token.reservedOp lexer

reserved :: (Monad m) => String -> ParsecT String u m ()
reserved = Token.reserved lexer

whiteSpace :: (Monad m) => ParsecT String u m ()
whiteSpace = Token.whiteSpace lexer
