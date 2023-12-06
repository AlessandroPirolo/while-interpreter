module Ast

// Abstract syntax tree for arithmetic expressions
type AExpr = 
        | AConst of int
        | Var of string
        | ABinOp of AExpr * string * AExpr

// Abstract syntax tree for boolean expressions
type BExpr = 
        | BConst of bool  
        | BUniOp of string * BExpr
        | BBinOp of BExpr * string * BExpr
        | BoolRelation of AExpr * string * AExpr

// Abstract syntax tree for the operations
type Statement = 
        | Assignment of string * AExpr
        | Skip
        | Composition of Statement * Statement
        | Conditional of BExpr * Statement * Statement
        | While of BExpr * Statement
        | Repeat of Statement * BExpr
        | For of string * AExpr * AExpr * Statement
        | PairAssign of string * string * AExpr * AExpr
        | OpAssign of string * string * AExpr

// A program is just a list of statment
type Prog = Prog of Statement list