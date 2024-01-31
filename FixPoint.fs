module FixPoint 

open Ast

let bottom = fun _ -> None

let id' (x : State) : State option = Some x

let cond (bexpr : State -> bool) (g : State -> State option) (g' : State -> State option) (state : State) : State option = 
    if bexpr state then g state else g' state 

let rec fn (f : (State -> State option) -> (State -> State option)) (n : int) arg =
    match n with 
    | 0 -> arg 
    | _ -> fn f (n - 1) (f arg)

let fix F (state : State) : State = 
    let rec find_fix n state = 
        let lub = fn F n bottom
        match lub state with
        | Some s -> s
        | None   -> find_fix (n+1) state
    find_fix 0 state

