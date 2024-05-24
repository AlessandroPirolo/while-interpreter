module Eval.EvalBexpr (
    evalBexpr
) where

import Parser.Ast as Ast ( AExpr, BExpr(..), State, Bop(..), RelOp(..) )
import Data.Map as Map ()
import Data.Maybe()
import Eval.EvalAexpr (evalAexpr)

evalBexpr :: BExpr -> State -> Bool
evalBexpr (BConst b)    _     = b
evalBexpr (BUnOp bexpr)      state = evalUnOp bexpr state
evalBexpr (BBinOp op l r) state = evalOper l op r state
evalBexpr (BoolRelation op l r) state = evalBoolOp l op r state

evalUnOp :: BExpr -> State -> Bool
evalUnOp bexpr state = let
    result = evalBexpr bexpr state
    in not result

evalOper :: BExpr -> Bop -> BExpr -> State -> Bool
evalOper l OpAnd r state = let
    left =  evalBexpr l state
    right = evalBexpr r state
    in (&&) left right
evalOper l OpOr r state = evalBexpr (BUnOp (BBinOp OpAnd (BUnOp l) (BUnOp r))) state

evalBoolOp :: AExpr -> RelOp -> AExpr -> State -> Bool
evalBoolOp l Leq r state = let
    left = evalAexpr l state
    right = evalAexpr r state
    in (<=) left right
evalBoolOp l Equ r state = let
    left = evalAexpr l state
    right = evalAexpr r state
    in (==) left right
evalBoolOp l Neq r state = evalBexpr (BUnOp (BoolRelation Equ l r)) state
evalBoolOp l Gg r state = evalBexpr (BUnOp (BoolRelation Leq l r)) state
evalBoolOp l Ll r state = evalBexpr (BUnOp (BoolRelation Leq r l)) state
evalBoolOp l Ll r state = evalBexpr (BoolRelation Leq r l) state 
