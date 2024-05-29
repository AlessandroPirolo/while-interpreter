{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Interp.Interp ( start ) where

import Parser.Ast ( Statement(..), State )
import Data.Map as Map ( fromListWith )
import Data.List as List ( map )
import Eval.Evaluator ( eval )
import Parser.Parser ( whileParser )
import Text.ParserCombinators.Parsec (parse)

start :: String -> State
start stmt = let
    result = case parse whileParser "" stmt of
        Right program -> program
        Left e -> error $ show e
    vars = findStateVar result
    state = createState vars
    in eval result state

findStateVar :: Statement -> [String]
findStateVar (Assignment var _)                 = [var]
findStateVar (OpAssignment _ var _)             = [var]
findStateVar Skip                               = []
findStateVar (Conditional _ t f)                = findStateVar t ++ findStateVar f
findStateVar (Composition (stmt:stmts))         = findStateVar stmt ++ findStateVar (Composition stmts)
findStateVar (Composition [])                   = []
findStateVar (While _ stmt)                     = findStateVar stmt
findStateVar (Repeat stmt _)                    = findStateVar stmt
findStateVar (For i _ _ stmt)                   = i : findStateVar stmt

createState :: [String] -> State
createState = Map.fromListWith (+) . List.map (\key -> (key, 0))
