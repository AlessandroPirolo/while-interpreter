module Parser.Ast (
  Program (..)
  , Statement (..)
  , BExpr (..)
  , AExpr (..)
  , State
  , Bop (..)
  , Aop (..)
  , RelOp (..)
  , OpAssOp (..)
) where

import Prelude
import Data.Map as Map ( Map )

type State = Map.Map String Integer

-- a program is just a statements
newtype Program = Program Statement
  deriving (Show, Eq)

-- Abstract syntax tree for statements
data Statement
  = Assignment String AExpr                       
  | PairAssignment (String, String) (AExpr, AExpr)  
  | Skip                                       
  | Composition [Statement]                    
  | Conditional BExpr Statement Statement       
  | While BExpr Statement                                           
  | Repeat Statement BExpr                     
  | For String AExpr AExpr Statement              
  | OpAssignment OpAssOp String AExpr          
  deriving (Show, Eq)

-- Abstract syntax tree for boolean expressions
data BExpr
  = BConst Bool                    
  | BUnOp BExpr         
  | BBinOp Bop BExpr BExpr  
  | BoolRelation RelOp AExpr AExpr  
  deriving (Show, Eq)

-- Abstract syntax tree for arithmetic expressions
data AExpr
  = AConst Integer                     
  | Var String                    
  | ABinOp Aop AExpr AExpr  
  deriving (Show, Eq)

data OpAssOp
  = Multeq
  | Inceq 
  | Deceq
  deriving (Show, Eq)

data Bop 
  = OpAnd 
  | OpOr 
  deriving (Show, Eq)

data Aop
  = Add 
  | Mult 
  | Minus 
  deriving (Show, Eq)

data RelOp
  = Ll 
  | Gg 
  | Leq
  | Geq 
  | Equ
  | Neq 
  deriving (Show, Eq)