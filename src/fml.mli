type fml_expression

type fml_value_binding

type fml_case

type fml_record_label_definition

type fml_pattern

type fml_pattern_desc

type fml_expression_desc =
  | Exp_ident of Path.t * Longident.t Asttypes.loc
  | Exp_constant of Asttypes.constant
  | Exp_let of Asttypes.rec_flag * fml_value_binding list * fml_expression
  | Exp_function of  { arg_label : Asttypes.arg_label; param : Ident.t;
     cases : fml_case list; partial : Typedtree.partial; }
  | Exp_apply of fml_expression * (Asttypes.arg_label * fml_expression option) list
  | Exp_match of fml_expression * fml_case list
  | Exp_tuple of fml_expression list
  | Exp_construct of Longident.t Asttypes.loc  *
     Types.constructor_description * fml_expression list
  | Exp_record of
   {
    fields: (Types.label_description * fml_record_label_definition) array;
    extended_expression: fml_expression option
   }
  | Exp_field of fml_expression * Types.label_description
  | Exp_array of fml_expression list
  | Exp_ifthenelse of fml_expression * fml_expression * fml_expression option
  | Exp_sequence of fml_expression * fml_expression
  | Exp_letexception of extension_constructor * fml_expression
  | Exp_unreachable

val ml2fml_exp : Typedtree.expression -> fml_expression

val ml2fml_exp_desc : Typedtree.expression_desc -> fml_expression_desc

val ml2fml_value_binding : Typedtree.value_binding -> fml_value_binding

val ml2fml_pattern : Typedtree.pattern -> fml_pattern

val ml2fml_pattern_desc : Typedtree.pattern_desc -> fml_pattern_desc

val ml2fml_record_label_definition :
  Typedtree.record_label_definition -> fml_record_label_definition
