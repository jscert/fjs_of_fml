type fml_expression = {
  exp_desc : fml_expression_desc;
  exp_loc : Location.t;
  exp_type : Types.type_expr;
  exp_env : Env.t;
  exp_attributes : Typedtree.attributes;
}
and fml_expression_desc =
    Exp_ident of Path.t * Longident.t Asttypes.loc
  | Exp_constant of Asttypes.constant
  | Exp_let of Asttypes.rec_flag * fml_value_binding list * fml_expression
  | Exp_function of { arg_label : Asttypes.arg_label; param : Ident.t;
      cases : fml_case list; partial : Typedtree.partial;
    }
  | Exp_apply of fml_expression *
      (Asttypes.arg_label * fml_expression option) list
  | Exp_match of fml_expression * fml_case list
  | Exp_tuple of fml_expression list
  | Exp_construct of Longident.t Asttypes.loc *
      Types.constructor_description * fml_expression list
  | Exp_record of {
      fields : (Types.label_description * fml_record_label_definition) array;
      extended_expression : fml_expression option;
    }
  | Exp_field of fml_expression * Types.label_description
  | Exp_array of fml_expression list
  | Exp_ifthenelse of fml_expression * fml_expression * fml_expression option
  | Exp_sequence of fml_expression * fml_expression
and fml_value_binding = {
  vb_pat : fml_pattern;
  vb_expr : fml_expression;
  vb_attributes : Typedtree.attributes;
  vb_loc : Location.t;
}
and fml_pattern = {
  pat_desc : fml_pattern_desc;
  pat_loc : Location.t;
  pat_type : Types.type_expr;
  mutable pat_env : Env.t;
  pat_attributes : Typedtree.attributes;
}
and fml_pattern_desc =
    Pat_any
  | Pat_var of Ident.t
  | Pat_tuple of fml_pattern list
  | Pat_constant of Asttypes.constant
  | Pat_construct of Longident.t Location.loc *
      Types.constructor_description * fml_pattern list
  | Pat_record of
      (Longident.t Location.loc * Types.label_description * fml_pattern) list *
      Asttypes.closed_flag
and fml_record_label_definition =
    Kept of Types.type_expr
  | Overridden of Longident.t Location.loc * fml_expression
and fml_case = {
  c_lhs : fml_pattern;
  c_guard : fml_expression option;
  c_rhs : fml_expression;
}
and fml_structure = {
  fstr_items : fml_structure_item list;
  fstr_type : Types.signature;
  fstr_final_env : Env.t;
}
and fml_structure_item = {
  fstr_desc : fml_structure_item_desc;
  fstr_loc : Location.t;
  fstr_env : Env.t;
}
and fml_structure_item_desc =
    Fml_tstr_eval of fml_expression
  | Fml_tstr_value of fml_value_binding list
  | Fml_tstr_type of fml_type_declaration list
  | Fml_attribute of Typedtree.attribute
  | Fml_tstr_open of Typedtree.open_description
and fml_type_declaration = {
  ftyp_name : string Location.loc;
  ftyp_type : Types.type_declaration;
}
and fml_type_kind = Fml_t_variant of Typedtree.constructor_declaration list
val ml2fml_exp_desc : Typedtree.expression_desc -> fml_expression_desc
val ml2fml_exp : Typedtree.expression -> fml_expression
val ml2fml_value_binding : Typedtree.value_binding -> fml_value_binding
val ml2fml_pattern : Typedtree.pattern -> fml_pattern
val ml2fml_pattern_desc : Typedtree.pattern_desc -> fml_pattern_desc
val ml2fml_exp_option : Typedtree.expression option -> fml_expression option
val ml2fml_record_label_definition :
  Typedtree.record_label_definition -> fml_record_label_definition
val ml2fml_case : Typedtree.case -> fml_case
val ml2fml_structure_item : Typedtree.structure_item -> fml_structure_item
val ml2fml_structure_item_desc :
  Typedtree.structure_item_desc -> fml_structure_item_desc
val ml2fml_type_declaration :
  Typedtree.type_declaration -> fml_type_declaration
val ml2fml_type_kind : Typedtree.type_kind -> fml_type_kind
val ml2fml_structure : Typedtree.structure -> fml_structure
