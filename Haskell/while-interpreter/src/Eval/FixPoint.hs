module Eval.FixPoint (
    fix
    , bottom
    , cond
    , id_p) where
import Parser.Ast ( State )

bottom :: State -> Maybe State
bottom = const Nothing

id_p :: State -> Maybe State
id_p = Just

cond :: (State -> Bool)
        -> (State -> Maybe State)
        -> (State -> Maybe State)
        -> State -> Maybe State
cond bexpr g g' state =
    if bexpr state
        then g state
        else g' state

fn :: ((State -> Maybe State) -> (State -> Maybe State))
      -> Integer -> (State -> Maybe State) -> (State -> Maybe State)
fn _ 0 = id
fn f n = f . fn f (n-1)

fix :: ((State -> Maybe State) -> (State -> Maybe State)) -> State -> State
fix f = lub f 0

lub :: ((State -> Maybe State) -> (State -> Maybe State)) -> Integer -> State -> State
lub f n state = case fn f n bottom state of
    (Just s) -> s
    Nothing -> lub f (n+1) state
