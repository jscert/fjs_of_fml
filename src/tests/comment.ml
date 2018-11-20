(* Some debug command 

See generated attributes :
ocamlfind ocamlc -dparsetree src/tests/comment.ml  2>&1 | grep attribute

*)



(** @elabel ECMAScript Reference Interpreter Implementation
    @esurl     https://tc39.github.io/ecma262/
    @esversion 2017
*)


type stack =
  | C of int * stack [@f value, stack]
  | N [@f]

(** Convert a data attribute into an accessor attribute.

    Implements the following spec text: "Convert the property \[...\] from a
    data property to an accessor property.  Preserve the existing values of the
    converted property's \[\[Configurable\]\] and \[\[Enumerable\]\] attributes
    and set the rest of the property's attributes to their default values."

    @essec 9.1.6.3-7.b.i *)
let push x stack = C(x, stack)

(** @essec 9.2.7.1
    @esid sec-%throwtypeerror% *)
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

(** foo foo  *)

(** PutValue(V,W)

    Note: Although the types of [v] and [w] are resultof, their states are
    disregarded and must be explicitly passed, as it is not clear in which
    order they were calculated.

    @esid sec-putvalue
    @essec 6.2.4.2
*)
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


