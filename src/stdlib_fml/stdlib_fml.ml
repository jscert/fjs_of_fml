(** OCaml Compatibility Wrapper for the Js_of_ocaml bis Standard Library

This implementation of the library permits programs designed to be compiled
with the limited standard library of Js_of_ocaml bis to be compiled against the
OCaml standard library for native/bytecode targets.

This file is to be compiled with the standard OCaml compiler with the
-nopervasives flag enabled. It should be linked with the OCaml standard
library into the end program file.
*)
let raise = Stdlib.raise;;
let failwith = Stdlib.failwith;;

(**{6 Boolean operations }*)
(** Note: Both OCaml and JS implement lazy evaluation for boolean operators. *)
let not = Stdlib.not;;
external ( && ) : bool -> bool -> bool = "%sequand";;
external ( || ) : bool -> bool -> bool = "%sequor";;

(**{6 Debugging }*)
external __LOC__ : string = "%loc_LOC"

(**{6 Integer arithmetic }*)
let ( + ) = Stdlib.( + );;
let ( - ) = Stdlib.( - );;
let ( * ) = Stdlib.( * );;
let ( / ) = Stdlib.( / );;

(**{6 Floating-point arithmetic }*)
let ( +. ) = Stdlib.( +. );;
let ( -. ) = Stdlib.( -. );;
let ( *. ) = Stdlib.( *. );;
let ( /. ) = Stdlib.( /. );;

(*
let ( ** ) = Stdlib.( ** );;
let atan = Stdlib.atan;;
let exp = Stdlib.exp;;
let log = Stdlib.log;;
let mod_float = Stdlib.mod_float;;
let float_of_int = Stdlib.float_of_int;;
let infinity = Stdlib.infinity;;
let neg_infinity = Stdlib.neg_infinity;;
let nan = Stdlib.nan;;
let max_float = Stdlib.max_float;;
let min_float = Stdlib.min_float;;
*)

(**{6 String operations }*)
(*
let (^) = Stdlib.(^);;
*)

(**{6 Character operations }*)
(*
let int_of_char = Stdlib.int_of_char;;
*)

(**{6 String conversion functions }*)
(*
let string_of_int = Stdlib.string_of_int;;
let string_of_float = Stdlib.string_of_float;;
let float_of_string = Stdlib.float_of_string;;
*)

(**{6 Input/output }*)
(*
let print_endline = Stdlib.print_endline;;
let prerr_string = Stdlib.prerr_string;;
let prerr_endline = Stdlib.prerr_endline;;
let prerr_newline = Stdlib.prerr_newline;;
*)

(**{6 References }*)
(** for future use for the global heap
let ref = Stdlib.ref;;
let (:=) = Stdlib.(:=);;
let (!) = Stdlib.(!);;
*)

(**{5 Pervasives-incompatible Definitions }

Functions in this section either deviate from the OCaml Pervasives type
signature, or are additional functions to fill in holes left by making a
polymorphic function not monomorphic.
*)

(**{6 Comparisons }*)
(** The standard comparison operators have been restricted to floating-point
operations only. *)
let ( = ) = Stdlib.( = );;
let ( < ) = Stdlib.( < );;
let ( > ) = Stdlib.( > );;
let ( <= ) = Stdlib.( <= );;
let ( >= ) = Stdlib.( >= );;

(*
let compare = Stdlib.compare;;
let min = Stdlib.min;;
let max = Stdlib.max;;
*)

let ( === ) = Stdlib.( = );;

(*
let float_compare = Stdlib.compare;;
*)
let int_eq = Stdlib.(=);;
let int_lt = Stdlib.(<);;
let int_gt = Stdlib.(>);;
let int_le = Stdlib.(<=);;
let int_ge = Stdlib.(>=);;
let int_compare = Stdlib.compare;;
let bool_eq = Stdlib.(=);;
let nat_eq = Stdlib.(=);;

let string_eq = Stdlib.(=);;
let string_lt = Stdlib.(<);;
let string_compare = Stdlib.compare;;

(**{6 Integer arithmetic }*)
(*let int_abs = Stdlib.abs;;*)

(**{6 Floating-point arithmetic }*)
(*
let fmod = Stdlib.mod_float;;
let float_neg = Stdlib.(~-.);;
let float_exp = Stdlib.exp;;
*)

(* Alan: Ideally we would add these to the spec, but for the moment conversion
   to a string is doing a foo+"", and conversion to an int is doing +foo *)
let int_of_number = Stdlib.int_of_float;;
let number_of_int = Stdlib.float_of_int;;

(**{6 String Operations }*)
(** String concatenation renamed from ^ *)
let strappend = Stdlib.(^);;

(**{5 String library }*)
(** Operations here are present in the OCaml standard library's String module
However cannot be used directly due to a potential module name collision when
linking to the standard library. (TODO: Work out if this can be avoided) *)

(*
This has the wrong typesignature for String.concat!
val string_concat : string -> string -> string (* + *)
*)

 let strlength = Stdlib.String.length;;
 let substring n m s = Stdlib.String.sub s n m;;
