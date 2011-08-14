{-#LANGUAGE TransformListComp #-}

module Language.Asdf.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Indent

import Control.Monad
import Control.Monad.Trans.State
import Control.Applicative ((<*))

import Data.Functor.Identity
import GHC.Exts


import Language.Asdf.Lexer
import Language.Asdf.AST
import Language.Asdf.Misc

type OperatorParser = Operator String Operators (StateT SourcePos Identity) ParsedExpr
newtype Operators = Operators [(OperatorParser, Rational)]

type Parser a = IndentParser String Operators a


parseSmplExpr :: Parser ParsedExpr
parseSmplExpr = parseId
            <|> indentParens lexer parseExpr
            <?> "simple expression"

parseStatement :: Parser ParsedStatement
parseStatement = parseAssign
             <|> parseIf
             <?> "statement"

parseId :: Parser ParsedExpr
parseId = parseExprWithPos $ liftM (Id . VarId) identifier


parseIf :: Parser ParsedStatement
parseIf = parseStatementWithPos $ do
  reserved "if"
  cond <- parseExpr
  thenBody <- indented >> block parseStatement
  elseBody <- optionMaybe $ do
    reserved "else"
    indented >> indented >> block parseStatement
  return $ If cond thenBody elseBody


parseAssign :: Parser ParsedStatement
parseAssign = parseStatementWithPos $ do
  -- same
  name <- try $ identifier <* reservedOp "="
  expr <- parseExpr
  return $ Assign name expr

parseOpDef :: Parser (OperatorParser, Rational)
parseOpDef = parseOpType "infixr"  InR  (flip Infix AssocRight . liftM call2)
         <|> parseOpType "infix"   In   (flip Infix AssocNone . liftM call2)
         <|> parseOpType "infixl"  InL  (flip Infix AssocLeft . liftM call2)
         <|> parseOpType "prefix"  Pre  (Prefix . liftM call)
         <|> parseOpType "postfix" Post (Postfix . liftM call)
         <?> "operator definition"
  where
    parseOpType tag fixityCons opCons = do
       reserved tag
       name <- operator
       precedence <- rational
       return (opCons $ parseExprWithPos $ reservedOp name >> return (Id $ OpId (fixityCons precedence) name), precedence)
    call f@(ParsedExpr pos _) a = ParsedExpr pos (Call f [a])
    call2 f@(ParsedExpr pos _) a b = ParsedExpr pos (Call f [a, b])

parseExpr :: Parser ParsedExpr
parseExpr = do
  Operators opList <- getState
  --let opTable = fmap (fmap fst) $ groupBy ((==) `on` snd) $ reverse $ sortBy (compare `on` snd) opList
  let opTable = [ op | (op, prec) <- opList, then reverse ... sortWith by prec, then group by prec]
  buildExpressionParser opTable parseSmplExpr <?> "expression"

parseExprWithPos :: Parser Expr -> Parser ParsedExpr
parseExprWithPos p = liftM2 ParsedExpr getPosition p

parseStatementWithPos :: Parser Statement -> Parser ParsedStatement
parseStatementWithPos p = liftM2 ParsedStatement getPosition p

parseLex :: Parser a -> Parser a
parseLex p = do 
  whiteSpace
  x <- p
  eof
  return x

parseSource :: Parser [ParsedStatement]
parseSource = parseLex $ many parseOpDef >>= putState . Operators >> block parseStatement

parseFile :: String -> IO (Either ParseError [ParsedStatement])
parseFile filename  =  do 
  input <- readFile filename
  -- the seq is here because of the stupid lazy IO! >:|
  return $ seq (length input) $ runIndent filename $ runParserT parseSource (Operators []) filename input




