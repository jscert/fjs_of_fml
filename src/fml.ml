open Typedtree

type miniml_expression =
  { exp_desc: miniml_expression_desc;
    exp_loc: Location.t;
    (*exp_extra: (exp_extra * Location.t * attributes) list;*)
    exp_type: Types.type_expr;
    exp_env: Env.t;
    exp_attributes: Typedtree.attributes;
   }

and miniml_expression_desc =
  | Exp_ident of Path.t * Longident.t Asttypes.loc
  | Exp_constant of Asttypes.constant
  | Exp_let of Asttypes.rec_flag * miniml_value_binding list * miniml_expression
  | Exp_function of  { arg_label : Asttypes.arg_label; param : Ident.t;
      cases : miniml_case list; partial : Typedtree.partial; }
  | Exp_apply of miniml_expression * (Asttypes.arg_label * miniml_expression option) list
  | Exp_match of miniml_expression * case list
(*| Exp_try *)
  | Exp_tuple of miniml_expression list 
  | Exp_construct of Longident.t Asttypes.loc  *
  Types.constructor_description * miniml_expression list 
(*| Exp_variant *)
  | Exp_record of
  {
  fields: (Types.label_description * miniml_record_label_definition) array ;
  (*representation: Types.record_representation ;*)
  extended_expression: miniml_expression option } 
  | Exp_field of miniml_expression * (*Longident.t Asttypes.loc  *)
  Types.label_description 
(*| Exp_setfield *)
  | Exp_array of miniml_expression list 
  | Exp_ifthenelse of miniml_expression * miniml_expression * miniml_expression option 
  | Exp_sequence of miniml_expression * miniml_expression 
(*| Exp_while of miniml_expression * miniml_expression 
  | Exp_for of Ident.t * Parsetree.pattern * miniml_expression * miniml_expression *
  Asttypes.direction_flag * miniml_expression 
| Exp_send | Exp_new | Exp_instvar | Exp_setinstvar | Exp_override | Exp_letmodule *)
  | Exp_letexception of extension_constructor * miniml_expression 
(*| Exp_assert | Exp_lazy | Exp_object | Exp_pack *)
  | Exp_unreachable

and miniml_value_binding =
  {
    vb_pat: miniml_pattern;
    vb_expr: miniml_expression;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }

and miniml_pattern =
  { pat_desc: miniml_pattern_desc;
    pat_loc: Location.t;
    (*pat_extra : (pat_extra * Location.t * attributes) list;*)
    pat_type: Types.type_expr;
    mutable pat_env: Env.t;
    pat_attributes: attributes;
   }

and miniml_pattern_desc =
  | Pat_any
  | Pat_var of Ident.t
(*| Tpat_alias | Tpat_constant | Tpat_tuple | Tpat_construct | Tpat_variant | Tpat_record
  | Tpat_array | Tpat_or of pattern  | Tpat_lazy *)

and miniml_record_label_definition =
  | MiniKept of Types.type_expr
  | MiniOverridden of Longident.t Location.loc * miniml_expression

and miniml_case =
    {
     c_lhs: miniml_pattern;
     c_guard: miniml_expression option;
     c_rhs: miniml_expression;
    }

let rec ml2mml_exp_desc = function
 | Texp_ident (path, ident, _) -> Exp_ident (path, ident)
 | Texp_constant c -> Exp_constant c
 | Texp_let (recur, vb_l, e) ->
    Exp_let (recur, List.map ml2mml_value_binding vb_l, ml2mml_exp e)
 | Texp_function { arg_label; param; cases; partial} ->
    Exp_function { arg_label; param ; cases = List.map ml2mmml_case cases; partial}
 | Texp_apply (e, l) ->
    Exp_apply (ml2mml_exp e, List.map (fun (al, eop) -> (al, ml2mml_exp_option eop)) l)
 | Texp_match (e, l, [], Total) -> Exp_match (ml2mml_exp e, l)
 | Texp_tuple l -> Exp_tuple (List.map ml2mml_exp l)
 | Texp_construct (i, cd, l) -> Exp_construct (i, cd, List.map ml2mml_exp l)
 | Texp_record { fields; extended_expression } ->
    Exp_record
     { fields = Array.map (fun (ld, rld) -> (ld, ml2mml_record_label_definition rld)) fields;
       extended_expression = ml2mml_exp_option extended_expression
     }
 | Texp_field (e, _, ld) -> Exp_field (ml2mml_exp e, ld)
 | Texp_array l -> Exp_array (List.map ml2mml_exp l)
 | Texp_ifthenelse (e0, e1, eop) ->
    Exp_ifthenelse (ml2mml_exp e0, ml2mml_exp e1, ml2mml_exp_option eop)
 | Texp_sequence (e0, e1) -> Exp_sequence (ml2mml_exp e0, ml2mml_exp e1)
 (* Unsupported Typedtree constructors *)
 | Texp_unreachable|Texp_letexception (_, _)|Texp_extension_constructor (_, _)
 |Texp_for (_, _, _, _, _, _) | Texp_while (_, _) |Texp_match (_, _, _, _)
 | Texp_try (_, _)|Texp_variant (_, _)  |Texp_setfield (_, _, _, _)
 | Texp_send (_, _, _)|Texp_new (_, _, _)| Texp_instvar (_, _, _)|Texp_setinstvar (_, _, _, _)
 |Texp_override (_, _)| Texp_letmodule (_, _, _, _) |Texp_assert _|Texp_lazy _
 |Texp_object (_, _)|Texp_pack _ -> assert false

and  ml2mml_exp { exp_desc; exp_loc; exp_extra; exp_type; exp_env; exp_attributes }
 =
  { exp_desc = ml2mml_exp_desc exp_desc;
    exp_loc; exp_type; exp_env; exp_attributes
  }

and ml2mml_value_binding  { vb_pat; vb_expr; vb_attributes; vb_loc }
=
 { vb_pat = ml2mml_pattern vb_pat;
   vb_expr = ml2mml_exp vb_expr;
   vb_attributes; vb_loc
 }

and ml2mml_pattern   { pat_desc; pat_loc; pat_extra;  pat_type; pat_env; pat_attributes }
=
  { pat_desc = ml2mml_pattern_desc pat_desc;
    pat_loc; pat_type; pat_env; pat_attributes
  }

and ml2mml_pattern_desc = function
 | Tpat_any -> Pat_any
 | Tpat_var (i, _) -> Pat_var i
 | _ -> assert false

and ml2mml_exp_option = function
 | None -> None
 | Some e -> Some (ml2mml_exp e)

and ml2mml_record_label_definition = function
 | Typedtree.Kept te -> MiniKept te
 | Typedtree.Overridden (i, e) -> MiniOverridden (i, ml2mml_exp e)

and ml2mmml_case { c_lhs; c_guard; c_rhs } =
 { c_lhs = ml2mml_pattern c_lhs;
   c_guard = ml2mml_exp_option c_guard;
   c_rhs = ml2mml_exp c_rhs
 }
