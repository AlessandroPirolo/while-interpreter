module Interp

open Ast
open Evaluator
open FSharp.Text.Lexing

let parse ds = 
        let lexbuf = LexBuffer<char>.FromString ds
        Parser.program Lexer.tokenize lexbuf

let find_state_var parsed_stmt = 
    let rec find_var stmt =
        match stmt with 
        | Skip -> 
                Set.empty

        | Assignment (var, _) -> 
                Set.singleton var
        
        | Composition (stmt1, stmt2) -> 
                Set.union (find_var stmt1) (find_var stmt2)
        
        | Conditional (_, then_stmt, else_stmt) ->
                Set.union (find_var then_stmt) (find_var else_stmt)
        
        | While (_, block) -> 
                find_var block
        
        | Repeat (block, _) -> 
                find_var block
        
        | For (_, _, _, block) -> 
                find_var block
        
        | PairAssign (var1, var2, _, _) -> 
                Set.union (Set.singleton var1) (Set.singleton var2)
        
        | OpAssign (var, _, _) -> 
                Set.singleton var
    
    find_var parsed_stmt
    |> Set.toList
    |> List.map( fun x -> (x,0))
    |> Map.ofList

let eval program state = 
    Evaluator.eval program state

let start stmt =
    let program = parse stmt

    (* Creating state entity which is a map key value, where
        keys are the variable identifier and values are default 
        values
        *)
    let state = find_state_var program

    eval program state