{-#LANGUAGE DeriveDataTypeable
          , DeriveFunctor
          , DeriveFoldable
          , DeriveTraversable #-}
module Language.Asdf.AST where

import Data.Data
import Data.Foldable
import Data.Traversable

import qualified Data.Map as Map

import Text.Parsec


data Prec = None
          | In Rational
          | InL Rational
          | InR Rational
          | Pre Rational
          | Post Rational
        deriving (Eq, Ord, Show, Data, Typeable)


data Identifier = VarId String
                | TypeId String
                | OpId Prec String
        deriving (Eq, Ord, Show, Data, Typeable)


data Statement = ExprStmt ParsedExpr
               | VarDef String
               | ConstDef String ParsedExpr
               | Foreign String
               | Assign String ParsedExpr
               | FuncDef String [ParsedStatement] Bool ParsedStatement
               | Scope [ParsedStatement]
               | For ParsedStatement ParsedStatement ParsedStatement ParsedStatement
               | While ParsedExpr ParsedStatement
               | DoWhile ParsedExpr ParsedStatement
               | If ParsedExpr [ParsedStatement] (Maybe [ParsedStatement])
               | Return ParsedExpr
  deriving (Show, Eq, Ord, Data, Typeable)

data ParsedStatement = ParsedStatement SourcePos Statement
  deriving (Show, Eq, Ord, Data, Typeable)

data Expr = Call ParsedExpr [ParsedExpr]
          {- This is more complex because we want to be able tell (when pretty printing) the difference between a
          regular identifier and an operator identifier. For example a + b, or zipWith((+), a, b)
          -}
          | Id Identifier  
          | Cast ParsedType ParsedExpr
          | Dot ParsedExpr String
          | StringLiteral String
          | CharLiteral Char
          | IntegerLiteral Integer
          | FloatLiteral Double
  deriving (Show, Eq, Ord, Data, Typeable)
        

data ParsedExpr = ParsedExpr SourcePos Expr
  deriving (Show, Eq, Ord, Data, Typeable)


data Type a = TypeName String
            | Pointer a
            | Array (Maybe Integer) a
            | TCall a [a]
            | Struct (Map.Map String a)
            | Union (Map.Map String a)
            | NType Int
            | Func [a] Bool a -- bool indicates variable number of arguments
            | DefaultInt
            | DefaultFloat
            | Var (Maybe String) 
           deriving (Show, Eq, Ord, Data, Typeable, Functor, Foldable, Traversable)

data ParsedType = ParsedType SourcePos (Type ParsedType)
  deriving (Show, Eq, Ord, Data, Typeable)




