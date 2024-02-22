module Eval.EvalAexpr (
    evalAexpr
) where

import Parser.Ast ( AExpr(..), State, Aop(..) )
import Data.Map as Map ( lookup )
import Data.Maybe (fromJust)

evalAexpr :: AExpr -> State -> Integer
evalAexpr (AConst num)    _     = num
evalAexpr (Var name)      state = evalVar name state
evalAexpr (ABinOp op l r) state = evalOper l op r state

evalVar :: String -> State -> Integer
evalVar name state = let
    result = fromJust (Map.lookup name state)
    in result

evalOper :: AExpr -> Aop -> AExpr -> State -> Integer
evalOper l op r state = let 
    left =  evalAexpr l state
    right = evalAexpr r state
    in arithOper op left right

arithOper :: Num a => Aop -> a -> a -> a
arithOper Add = (+)
arithOper Mult = (*)
arithOper Minus = (-)