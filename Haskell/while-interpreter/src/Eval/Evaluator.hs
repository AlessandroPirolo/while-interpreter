module Eval.Evaluator (eval) where

import Parser.Ast ( AExpr (Var, AConst, ABinOp), BExpr (BoolRelation, BUnOp), Statement(..), State, OpAssOp(..), RelOp (Ll), Aop(..) )
import Data.Map as Map ( insert, lookup )
--import Data.Function (id)
import Eval.EvalAexpr (evalAexpr)
import Eval.EvalBexpr (evalBexpr)
import Eval.FixPoint ( id_p, fix, cond )
import Data.Maybe (fromJust)

eval :: Statement -> State -> State
eval (Assignment var aexpr)                         = evalAssignment var aexpr
eval (OpAssignment op var aexpr)                    = evalOpAssignment var op aexpr
eval Skip                                           = id
eval (Conditional bexpr t f)                        = evalConditional bexpr t f
eval (Composition stmts)                            = evalComposition stmts
eval (While bexpr stmt)                             = evalWhile bexpr stmt
eval (Repeat stmt bexpr)                            = evalRepeat stmt bexpr
eval (For i to from e)                              = evalFor i to from e

evalAssignment :: String -> AExpr -> State -> State
evalAssignment var aexpr state = let
    result = evalAexpr aexpr state
    state' = insert var result state
    in state'

evalOpAssignment :: String -> OpAssOp -> AExpr -> State -> State
evalOpAssignment var op aexpr state = let
    oper = opAssign op
    stmt = Assignment var (ABinOp oper (Var var) aexpr)
    in eval stmt state

evalConditional :: BExpr -> Statement -> Statement -> State -> State
evalConditional bexpr t f state =
    if evalBexpr bexpr state
        then eval t state
        else eval f state

evalComposition :: [Statement] -> State -> State
evalComposition stmts state = foldl (flip eval) state stmts

evalWhile :: BExpr -> Statement -> State -> State
evalWhile bexpr stmt state = let
    expr = evalBexpr bexpr
    f g = cond expr (g . eval stmt) id_p
    in fix f state

evalRepeat :: Statement -> BExpr -> State -> State
evalRepeat stmt bexpr state = let
    desugared = While (BUnOp bexpr) (stmt) 
    in eval desugared state

evalFor :: String -> AExpr -> AExpr -> Statement -> State -> State
evalFor i to from e state = let
    desugared = Composition [Assignment i to, While (BoolRelation Ll (Var i) from) (Composition [e,OpAssignment Inceq i (AConst 1)])]
    in eval desugared state

opAssign :: OpAssOp -> Aop
opAssign Inceq = Add
opAssign Multeq = Mult
opAssign Deceq = Minus
