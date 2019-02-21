open Typedtree
open Visitor

type expression_desc =
  | Exp_ident of Path.t * Longident.t Asttypes.loc * Types.value_description 
  | Exp_constant of Asttypes.constant 
  | Exp_let of Asttypes.rec_flag * value_binding list * expression 
(*| Exp_function of
  {
  arg_label: Asttypes.arg_label ;
  param: Ident.t ;
  cases: case list ;
  partial: partial }*)
  | Exp_apply of expression * (Asttypes.arg_label * expression option) list
  | Exp_match of expression * case list * case list * partial 
(*| Exp_try of expression * case list *)
  | Exp_tuple of expression list 
  | Exp_construct of Longident.t Asttypes.loc  *
  Types.constructor_description * expression list 
(*| Exp_variant of Asttypes.label * expression option *)
  | Exp_record of
  {
  fields: (Types.label_description * record_label_definition) array ;
  representation: Types.record_representation ;
  extended_expression: expression option } 
  | Exp_field of expression * Longident.t Asttypes.loc  *
  Types.label_description 
(*| Exp_setfield of expression * Longident.t Asttypes.loc  *
  Types.label_description * expression *)
  | Exp_array of expression list 
  | Exp_ifthenelse of expression * expression * expression option 
  | Exp_sequence of expression * expression 
  | Exp_while of expression * expression 
  | Exp_for of Ident.t * Parsetree.pattern * expression * expression *
  Asttypes.direction_flag * expression 
(*| Exp_send of expression * meth * expression option 
  | Exp_new of Path.t * Longident.t Asttypes.loc  * Types.class_declaration 
  | Exp_instvar of Path.t * Path.t * string Asttypes.loc
  | Exp_setinstvar of Path.t * Path.t * string Asttypes.loc * expression 
  | Exp_override of Path.t * (Path.t * string Asttypes.loc  * expression)
  list 
  | Exp_letmodule of Ident.t * string Asttypes.loc  * module_expr *
  expression *)
  | Exp_letexception of extension_constructor * expression 
(*| Exp_assert of expression 
  | Exp_lazy of expression 
  | Exp_object of class_structure * string list 
  | Exp_pack of module_expr *)
  | Exp_unreachable 
  | Exp_extension_constructor of Longident.t Asttypes.loc * Path.t
