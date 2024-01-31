module Evaluator

open Ast
open FixPoint

let rec eval_aexpr (aexpr : AExpr) (state : State) : int = 
    match aexpr with
    | AConst num        -> num
    | Var name          -> Map.find name state
    | ABinOp (l, op, r) -> 
        let left = eval_aexpr l state
        let right = eval_aexpr r state
        match op with 
        | "+" -> left + right
        | "-" -> left - right
        | "*" -> left * right
        | _   -> left // error

let eval_assignment (var : string) (aexpr : AExpr) (state : State) : State = 
    let result = eval_aexpr aexpr state
    Map.add var result state

let eval_pair_assign (var1 : string) (var2 : string) (expr1 : AExpr) (expr2 : AExpr) (state : State) : State =
    let result1 = eval_aexpr expr1 state
    let result2 = eval_aexpr expr2 state
    let state1 = Map.add var1 result1 state
    Map.add var2 result2 state1

let eval_opassign (var : string) (op : string) (aexpr : AExpr) (state : State) : State =
    let value = Map.find var state 
    let expr_evalued = eval_aexpr aexpr state
    let result = 
        match op with
        | "+=" -> value + expr_evalued
        | "-=" -> value - expr_evalued
        | "*=" -> value * expr_evalued
        | _   -> value // error
    
    Map.add var result state

let rec eval_bexpr (bexpr : BExpr) (state : State) : bool = 
    match bexpr with
    | BConst b -> b
    | BUniOp (_, bexpr) ->
        let result = eval_bexpr bexpr state
        not result
    | BBinOp (l, op, r) ->
        let left = eval_bexpr l state
        let right = eval_bexpr r state
        match op with
        | "&&" -> left && right
        | "||" -> left || right
        | _ -> true
    | BoolRelation (l, op, r) ->
        let left = eval_aexpr l state
        let right = eval_aexpr r state
        match op with 
        | "<=" -> left <= right
        | ">=" -> left >= right
        | "<"  -> left < right
        | ">"  -> left > right
        | "="  -> left = right
        | "!=" -> left <> right
        | _    -> true // error 
    
let rec eval (stmt : Statement) (state : State) : State =
    match stmt with 
    | Skip -> 
            state

    | Assignment (var, expr) ->
            eval_assignment var expr state
    
    | PairAssign (var1, var2, expr1, expr2) ->
            eval_pair_assign var1 var2 expr1 expr2 state
    
    | OpAssign (var, operator, expr) ->
            eval_opassign var operator expr state
    
    | Composition (stmt1, stmt2) ->
            let state1 = eval stmt1 state
            eval stmt2 state1
    
    | Conditional (expr, true_stmt, false_stmt) ->
            let bexpr = eval_bexpr expr state
            match bexpr with
            | true  -> eval true_stmt state
            | false -> eval false_stmt state
    
    | While (bexpr, stmt) ->
            eval_while bexpr stmt state
            
    | Repeat (stmt, bexpr) ->
            eval_repeat bexpr stmt state

    | For (var, expr1, expr2, stmt) ->
            eval_for var expr1 expr2 stmt state
      


and eval_while (bexpr : BExpr) (stmt : Statement) (state : State) : State = 
    let expr = eval_bexpr bexpr
    let F g = cond expr (g << eval stmt)  id'
    fix F state

and eval_repeat (bexpr : BExpr) (stmt : Statement) (state : State) : State =
    let expr = eval_bexpr bexpr
    let F g = (cond expr id' g) << eval stmt
    fix F state

and eval_for var expr1 expr2 stmt state = state
