module Eval.Evaluator (eval) where

import Parser.Ast ( AExpr (Var, AConst), BExpr (BoolRelation), Statement(..), State, OpAssOp(..), RelOp (Ll) )
import Data.Map as Map ( insert, lookup )
--import Data.Function (id)
import Eval.EvalAexpr (evalAexpr)
import Eval.EvalBexpr (evalBexpr)
import Eval.FixPoint ( id_m, fix, cond )
import Data.Maybe (fromJust)

eval :: Statement -> State -> State
eval (Assignment var aexpr)                         = evalAssignment var aexpr
eval (PairAssignment (var1, var2) (aexpr1, aexpr2)) = evalPairAssign var1 var2 aexpr1 aexpr2
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

evalPairAssign :: String -> String -> AExpr -> AExpr -> State -> State
evalPairAssign var1 var2 aexpr1 aexpr2 state = let
    val1 = evalAexpr aexpr1 state
    state' = insert var1 val1 state
    val2 = evalAexpr aexpr2 state'
    state'' = insert var2 val2 state''
    in state''

evalOpAssignment :: String -> OpAssOp -> AExpr -> State -> State
evalOpAssignment var op aexpr state = let
    result = evalAexpr aexpr state
    result' = opAssign var op result state
    state' = insert var result' state
    in state'

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
    f g = cond expr (g . eval stmt) id'
    in fix f state

evalRepeat :: Statement -> BExpr -> State -> State
evalRepeat stmt bexpr state = let
    expr = evalBexpr bexpr
    f g = cond expr id' g . eval stmt
    in fix f state

evalFor :: String -> AExpr -> AExpr -> Statement -> State -> State
evalFor i to from e state = let
    desugared = Composition [Assignment i to, While (BoolRelation Ll (Var i) from) (Composition [e,OpAssignment Inceq i (AConst 1)])]
    in eval desugared state

opAssign :: String -> OpAssOp -> Integer -> State -> Integer
opAssign var Inceq val state = let
    v = fromJust (Map.lookup var state)
    result = v + val
    in result
opAssign var Multeq val state = let
    v = fromJust (Map.lookup var state)
    result = v * val
    in result
opAssign var Deceq val state = let
    v = fromJust (Map.lookup var state)
    result = v - val
    in result
