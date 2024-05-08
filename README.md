# While Interpreter
## Contents
- [Description](#description)
- [How to use it](#how-to-use-it)
- [While+ language](#while+-language)

## Description
As the name suggest, the following project is an interpreter for the denotational semantics of the **While+** language. 
The program takes as input any statement and some representation of a state, and produces an output state according to the evalutation rules of While+.

A proper attention must be given to the while statement: its evaluation relies on **Kleene-Knaster-Tarski** fixpoint iteration sequence. This means that if the
sequence is infinite and so non-terminating, the program does not terminate either (it keeps running).

## How to use it
1. Download the repo
2. Run `.\build.ps1`
3. Run `.\WhileAbstractInterpreter.exe "file_name.txt" "lower bound" "upper bound"`

## While+ Language
The While syntax is the following:
```
S ::= x := a | skip | S1; S2
  | if b then S1 else S2
  | while b do S
```
where `a` stands for arithmentic expression and `b` for boolean expression:
```
a ::= n | x | a1 + a2
  | a1 ∗ a2 | a1 − a2

b ::= true | false 
  | b1 ∧ b2 | ¬ b | b1 ∧ b2
  | a1 = a2 | a1 <= a2
  | a1 < a2 | a1 > a2
  | a1 >= a2 | a1 != a2 
```
What makes While be While+ are the following expression:
```
S ::= ... | x += a | x-= a | x *= a
  | x, y := a1, a2
  | repeat S until b
  | for i := a1 to a2 do S 
```
These are just syntactic sugars:
| Sugared                   | Desugared                                 |
| ------------------------- | ----------------------------------------- |
| `x,y := a1,a2`            | `x:=a1;y:=a2`                             |
| `x += a`                  | `x:= x + a`                               |
| `x -= a`                  | `x:= x - a`                               |
| `x *= a`                  | `x:= x * a`                               |
| `repeat S until b`        | `while ¬b do S`                           |
| `for i := a1 to a2 do S ` | `i := a1;while i < a2 do (S; i := i + 1)` |




