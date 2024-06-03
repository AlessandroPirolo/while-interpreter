{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Interp.Interp ( start ) where

import Parser.Ast ( Statement(..), State, AExpr(..), BExpr(..) )
import Data.Map as Map ( fromListWith, empty )
import Data.List as List ( map, insert )
import Eval.Evaluator ( eval )
import Parser.Parser ( whileParser )
import Text.ParserCombinators.Parsec (parse)

start :: String -> Either State (Bool, String)
start stmt = let
    result = parse whileParser "" stmt
    in case result of
      Left err -> Right (False, show err)
      Right program -> 
          let (check, s) = checkUndecVar program []
          in if check 
            then Right (False, s)
            else Left (eval program empty) 

checkAexpr :: AExpr -> [String] -> (Bool, String)
checkAexpr (Var s) m = if s `elem` m then (True, s) else (False, "") 
checkAexpr (ABinOp _ a1 a2) m = 
      let (res, s) = checkAexpr a1 m 
      in if res then (res, s) else checkAexpr a2 m 
checkAexpr _ m = (False, "")

checkBexpr :: BExpr -> [String] -> (Bool, String)
checkBexpr (BoolRelation _ a1 a2) m  = let 
      (res, s) = checkAexpr a1 m 
      in if res then (res, s) else checkAexpr a2 m 
checkBexpr _ _                       = (False, "")


checkUndecVar :: Statement -> [String] -> (Bool, String)
checkUndecVar (Assignment v a) m                  = checkAexpr a (insert v m) 
checkUndecVar (OpAssignment _ _ a) m              = checkAexpr a m
checkUndecVar Skip _                              = (False, "")
checkUndecVar (Conditional b t f) m               = let
      (res, s) = checkBexpr b m 
      in if res then (res, s) else checkUndecVar t m
checkUndecVar (Composition (stmt:stmts)) m        = let
      (res, s) = checkUndecVar stmt m 
      in if res then (res, s) else checkUndecVar (Composition stmts) m
checkUndecVar (Composition []) m                  = (False, "")
checkUndecVar (While b stmt) m                    = let 
      (res, s) = checkBexpr b m 
      in if res then (res, s) else checkUndecVar stmt m
checkUndecVar (Repeat stmt b) m                   = let
      (res, s) = checkBexpr b m 
      in if res then (res, s) else checkUndecVar stmt m
checkUndecVar (For i _ _ stmt) m                  = checkUndecVar stmt m

createState :: [String] -> State
createState = Map.fromListWith (+) . List.map (\key -> (key, 0))
