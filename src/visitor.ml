(* Test : 'ocamlbuild -I src -use-ocamlfind visitor.native -cflag -dsource'  *)

open Typedtree

type expression_desc = Typedtree.expression_desc =
  | Texp_ident of (Path.t [@name "pt"]) * (Longident.t [@name "lt"]) Asttypes.loc * Types.value_description 
  | Texp_constant of Asttypes.constant 
  | Texp_let of Asttypes.rec_flag * value_binding list * expression 
  | Texp_function of
  {
  arg_label: Asttypes.arg_label ;
  param: (Ident.t [@name "it"]) ;
  cases: case list ;
  partial: partial }
  | Texp_apply of expression * (Asttypes.arg_label * expression option) list
  | Texp_match of expression * case list * case list * partial 
  | Texp_try of expression * case list 
  | Texp_tuple of expression list 
  | Texp_construct of (Longident.t [@name "lt"]) Asttypes.loc  *
  Types.constructor_description * expression list 
  | Texp_variant of Asttypes.label * expression option 
  | Texp_record of
  {
  fields: (Types.label_description * record_label_definition) array ;
  representation: Types.record_representation ;
  extended_expression: expression option } 
  | Texp_field of expression * (Longident.t [@name "lt"]) Asttypes.loc  *
  Types.label_description 
  | Texp_setfield of expression * (Longident.t [@name "lt"]) Asttypes.loc  *
  Types.label_description * expression 
  | Texp_array of expression list 
  | Texp_ifthenelse of expression * expression * expression option 
  | Texp_sequence of expression * expression 
  | Texp_while of expression * expression 
  | Texp_for of (Ident.t [@name "it"]) * Parsetree.pattern * expression * expression *
  Asttypes.direction_flag * expression 
  | Texp_send of expression * meth * expression option 
  | Texp_new of(Path.t [@name "pt"]) * (Longident.t [@name "lt"]) Asttypes.loc  * Types.class_declaration 
  | Texp_instvar of (Path.t [@name "pt"]) * (Path.t [@name "pt"]) * (string Asttypes.loc [@opaque]) 
  | Texp_setinstvar of (Path.t [@name "pt"]) * (Path.t [@name "pt"]) * (string Asttypes.loc [@opaque]) * expression 
  | Texp_override of (Path.t [@name "pt"]) * ((Path.t [@name "pt"]) * (string Asttypes.loc [@opaque]) * expression)
  list 
  | Texp_letmodule of (Ident.t [@name "it"]) * (string Asttypes.loc [@opaque]) * module_expr *
  expression 
  | Texp_letexception of extension_constructor * expression 
  | Texp_assert of expression 
  | Texp_lazy of expression 
  | Texp_object of class_structure * string list 
  | Texp_pack of module_expr 
  | Texp_unreachable 
  | Texp_extension_constructor of (Longident.t [@name "lt"]) Asttypes.loc * (Path.t [@name "pt"])
  [@@deriving visitors { variety = "map" }]
