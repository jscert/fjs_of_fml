open Typedtree

type fml_expression =
  { exp_desc: fml_expression_desc;
    exp_loc: Location.t;
    (*exp_extra: (exp_extra * Location.t * attributes) list;*)
    exp_type: Types.type_expr;
    exp_env: Env.t;
    exp_attributes: Typedtree.attributes;
   }

and fml_expression_desc =
  | Exp_ident of Path.t * Longident.t Asttypes.loc
  | Exp_constant of Asttypes.constant
  | Exp_let of Asttypes.rec_flag * fml_value_binding list * fml_expression
  | Exp_function of  { arg_label : Asttypes.arg_label; param : Ident.t;
      cases : fml_case list; partial : Typedtree.partial; }
  | Exp_apply of fml_expression * (Asttypes.arg_label * fml_expression option) list
  | Exp_match of fml_expression * fml_case list
(*| Exp_try *)
  | Exp_tuple of fml_expression list 
  | Exp_construct of Longident.t Asttypes.loc  *
  Types.constructor_description * fml_expression list 
(*| Exp_variant *)
  | Exp_record of
  {
  fields: (Types.label_description * fml_record_label_definition) array ;
  (*representation: Types.record_representation ;*)
  extended_expression: fml_expression option } 
  | Exp_field of fml_expression * (*Longident.t Asttypes.loc  *)
  Types.label_description 
(*| Exp_setfield *)
  | Exp_array of fml_expression list 
  | Exp_ifthenelse of fml_expression * fml_expression * fml_expression option 
  | Exp_sequence of fml_expression * fml_expression 
(*| Exp_while of fml_expression * fml_expression 
  | Exp_for of Ident.t * Parsetree.pattern * fml_expression * fml_expression *
  Asttypes.direction_flag * fml_expression 
| Exp_send | Exp_new | Exp_instvar | Exp_setinstvar | Exp_override | Exp_letmodule *)
  | Exp_letexception of extension_constructor * fml_expression 
(*| Exp_assert | Exp_lazy | Exp_object | Exp_pack *)
  | Exp_unreachable

and fml_value_binding =
  {
    vb_pat: fml_pattern;
    vb_expr: fml_expression;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and fml_pattern =
  { pat_desc: fml_pattern_desc;
    pat_loc: Location.t;
    (*pat_extra : (pat_extra * Location.t * attributes) list;*)
    pat_type: Types.type_expr;
    mutable pat_env: Env.t;
    pat_attributes: attributes;
   }

and fml_pattern_desc =
  | Pat_any
  | Pat_var of Ident.t
  | Pat_tuple of fml_pattern list
(*| Tpat_alias | Tpat_constant | Tpat_construct | Tpat_variant | Tpat_record
  | Tpat_array | Tpat_or of pattern  | Tpat_lazy *)

and fml_record_label_definition =
  | MiniKept of Types.type_expr
  | MiniOverridden of Longident.t Location.loc * fml_expression

and fml_case =
    {
     c_lhs: fml_pattern;
     c_guard: fml_expression option;
     c_rhs: fml_expression;
    }

let rec ml2fml_exp_desc = function
 | Texp_ident (path, ident, _) -> Exp_ident (path, ident)
 | Texp_constant c -> Exp_constant c
 | Texp_let (recur, vb_l, e) ->
    Exp_let (recur, List.map ml2fml_value_binding vb_l, ml2fml_exp e)
 | Texp_function { arg_label; param; cases; partial} ->
    Exp_function { arg_label; param ; cases = List.map ml2fml_case cases; partial}
 | Texp_apply (e, l) ->
    Exp_apply (ml2fml_exp e, List.map (fun (al, eop) -> (al, ml2fml_exp_option eop)) l)
 | Texp_match (e, l, [], Total) -> Exp_match (ml2fml_exp e, l)
 | Texp_tuple l -> Exp_tuple (List.map ml2fml_exp l)
 | Texp_construct (i, cd, l) -> Exp_construct (i, cd, List.map ml2fml_exp l)
 | Texp_record { fields; extended_expression } ->
    Exp_record
     { fields = Array.map (fun (ld, rld) -> (ld, ml2fml_record_label_definition rld)) fields;
       extended_expression = ml2fml_exp_option extended_expression
     }
 | Texp_field (e, _, ld) -> Exp_field (ml2fml_exp e, ld)
 | Texp_array l -> Exp_array (List.map ml2fml_exp l)
 | Texp_ifthenelse (e0, e1, eop) ->
    Exp_ifthenelse (ml2fml_exp e0, ml2fml_exp e1, ml2fml_exp_option eop)
 | Texp_sequence (e0, e1) -> Exp_sequence (ml2fml_exp e0, ml2fml_exp e1)
 (* Unsupported Typedtree constructors *)
 | Texp_unreachable|Texp_letexception (_, _)|Texp_extension_constructor (_, _)
 |Texp_for (_, _, _, _, _, _) | Texp_while (_, _) |Texp_match (_, _, _, _)
 | Texp_try (_, _)|Texp_variant (_, _)  |Texp_setfield (_, _, _, _)
 | Texp_send (_, _, _)|Texp_new (_, _, _)| Texp_instvar (_, _, _)|Texp_setinstvar (_, _, _, _)
 |Texp_override (_, _)| Texp_letmodule (_, _, _, _) |Texp_assert _|Texp_lazy _
 |Texp_object (_, _)|Texp_pack _ -> assert false

and  ml2fml_exp { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes }
 =
  { exp_desc = ml2fml_exp_desc exp_desc;
    exp_loc; exp_type; exp_env; exp_attributes
  }

and ml2fml_value_binding  { vb_pat; vb_expr; vb_attributes; vb_loc }
=
 { vb_pat = ml2fml_pattern vb_pat;
   vb_expr = ml2fml_exp vb_expr;
   vb_attributes; vb_loc
 }

and ml2fml_pattern   { pat_desc; pat_loc; pat_extra;  pat_type; pat_env; pat_attributes }
=
  { pat_desc = ml2fml_pattern_desc pat_desc;
    pat_loc; pat_type; pat_env; pat_attributes
  }

and ml2fml_pattern_desc = function
 | Tpat_any -> Pat_any
 | Tpat_var (i, _) -> Pat_var i
 | _ -> assert false

and ml2fml_exp_option = function
 | None -> None
 | Some e -> Some (ml2fml_exp e)

and ml2fml_record_label_definition = function
 | Typedtree.Kept te -> MiniKept te
 | Typedtree.Overridden (i, e) -> MiniOverridden (i, ml2fml_exp e)

and ml2fml_case { c_lhs; c_guard; c_rhs } =
 { c_lhs = ml2fml_pattern c_lhs;
   c_guard = ml2fml_exp_option c_guard;
   c_rhs = ml2fml_exp c_rhs
 }
