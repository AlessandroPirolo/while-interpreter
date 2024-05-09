module Iteractive.Pretty (prettyMap, prettyItMode) where
import Data.Map (toList)
import Parser.Ast (State)

prettyRow :: (Show a1, Show a2) => [(a1, a2)] -> [Char]
prettyRow list = case list of
    [] -> "\nDone."
    ((k,v):rest) -> show k 
                    ++ "       |      " 
                    ++ show v 
                    ++ "\n" 
                    ++ prettyRow rest

prettyMap :: State -> String
prettyMap m = if null m
                    then "" 
                    else "Variables | Values \n" 
                         ++ "------------------ \n" 
                         ++ prettyRow (toList m)

prettyItMode :: [Char]
prettyItMode = "\ESC[92mEntering iteractive mode...\n" ++ "\ESC[0mdigit \ESC[1m:q" ++ 
               " \ESC[0mor " ++ "\ESC[1m:quit " ++ "\ESC[0mto exit, press " ++ "\ESC[1mENTER " ++ "\ESC[0mto evaluate"