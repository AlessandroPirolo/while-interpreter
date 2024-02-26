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
evalOper l op r state = let
    left =  evalBexpr l state
    right = evalBexpr r state
    in boolOper op left right

evalBoolOp :: AExpr -> RelOp -> AExpr -> State -> Bool
evalBoolOp l op r state = let
    left = evalAexpr l state
    right = evalAexpr r state
    in boolRel op left right


boolOper :: Bop -> Bool -> Bool -> Bool
boolOper OpAnd = (&&)
boolOper OpOr = (||)


boolRel :: Ord a => RelOp -> a -> a -> Bool
boolRel Leq = (<=)
boolRel Geq = (>=) 
boolRel Gg = (>) 
boolRel Ll = (<) 
boolRel Equ = (==) 
boolRel Neq = (/=) 