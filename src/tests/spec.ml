(** @elabel ECMAScript Reference Interpreter Implementation
    @esurl     https://tc39.github.io/ecma262/
    @esversion 2017
*)

type stack =
  | C of int * stack [@f value, stack]
  | N [@f]

let is_empty s = s === N

let push x stack = C(x, stack)

let pop stack =
  match stack with
  | C (x, xs) -> x
  | N -> failwith "Empty list"

(**
 blah
 foo
 bar
 @esid ride-the-lightning

*)

type expr =
  | Const of int [@f value]
  | Add of expr * expr [@f left, right]
  | Sub of expr * expr [@f left, right]
  | Mul of expr * expr [@f left, right]
  | Div of expr * expr [@f left, right]
  | Pop of sexpr [@f stack]
and sexpr =
  | Emp [@f]
  | Push of expr * sexpr [@f value, stack]

let rec run expr = match expr with
  | Const n -> n
  | Add (ls, rs) -> run ls + run rs
  | Sub (ls, rs) -> run ls - run rs
  | Mul (ls, rs) -> run ls * run rs
  | Div (ls, rs) -> run ls / run rs
  | Pop s -> pop (evals s)
and evals sexpr = match sexpr with
  | Emp -> N
  | Push (v, s) -> push (run v) (evals s)

let rec mapMystack f s = match s with
  | N -> N
  | C (x, xs) -> C (f x, mapMystack f xs)
