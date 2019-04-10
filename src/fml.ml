open Typedtree
open Mytools

(* Types  *)

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
    { fields: (Types.label_description * fml_record_label_definition) array ;
      (*representation: Types.record_representation ;*)
      extended_expression: fml_expression option
    } 
  | Exp_field of fml_expression * (*Longident.t Asttypes.loc  *)
  Types.label_description 
(*| Exp_setfield *)
  | Exp_array of fml_expression list 
  | Exp_ifthenelse of fml_expression * fml_expression * fml_expression option 
  | Exp_sequence of fml_expression * fml_expression 
(*| Exp_while of fml_expression * fml_expression 
  | Exp_for of Ident.t * Parsetree.pattern * fml_expression * fml_expression *
  Asttypes.direction_flag * fml_expression 
  | Exp_send | Exp_new | Exp_instvar | Exp_setinstvar | Exp_override | Exp_letmodule 
  | Exp_letexception of extension_constructor * fml_expression 
  | Exp_assert | Exp_lazy | Exp_object | Exp_pack 
  | Exp_unreachable*)

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
  | Pat_constant of Asttypes.constant
  | Pat_construct of Longident.t Location.loc *
     Types.constructor_description * fml_pattern list
  | Pat_record of (Longident.t Location.loc * Types.label_description * fml_pattern) list *
        Asttypes.closed_flag 
(*| Tpat_alias | Tpat_constant | Tpat_construct | Tpat_variant 
  | Tpat_array | Tpat_or of pattern  | Tpat_lazy *)

and fml_record_label_definition =
  | Kept of Types.type_expr
  | Overridden of Longident.t Location.loc * fml_expression

and fml_case =
    {
     c_lhs: fml_pattern;
     c_guard: fml_expression option;
     c_rhs: fml_expression;
    }

and fml_structure =
  { fstr_items : fml_structure_item list;
    fstr_type : Types.signature;
    fstr_final_env : Env.t;
  }

and fml_structure_item =
  { fstr_desc : fml_structure_item_desc;
    fstr_loc : Location.t;
    fstr_env : Env.t
  }

and fml_structure_item_desc =
  | Fml_tstr_eval of fml_expression
  | Fml_tstr_value of fml_value_binding list
  | Fml_tstr_type of  fml_type_declaration list
  | Fml_attribute of Typedtree.attribute
  | Fml_tstr_open of Typedtree.open_description

and fml_type_declaration = 
 { ftyp_name : string Location.loc;
   ftyp_type : Types.type_declaration;
 }

and fml_type_kind = Fml_t_variant of Typedtree.constructor_declaration list

(* Conversion  *)
let rec ml2fml_exp_desc = function
 | Texp_ident (path, ident, _) -> Exp_ident (path, ident)
 | Texp_constant c -> Exp_constant c
 | Texp_let (recur, vb_l, e) ->
    Exp_let (recur, List.map ml2fml_value_binding vb_l, ml2fml_exp e)
 | Texp_function { arg_label; param; cases; partial} ->
    Exp_function { arg_label; param ; cases = List.map ml2fml_case cases; partial}
 | Texp_apply (e, l) ->
    Exp_apply (ml2fml_exp e, List.map (fun (al, eop) -> (al, ml2fml_exp_option eop)) l)
 | Texp_match (e, l, [], Total) -> Exp_match (ml2fml_exp e, List.map ml2fml_case l)
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
 |Texp_object (_, _)|Texp_pack _ -> let loc = Location.none in out_of_scope loc "exp_desc"

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
 | Tpat_constant c -> Pat_constant c
 | Tpat_construct (i, cd, pl) ->
    Pat_construct (i, cd, List.map ml2fml_pattern pl)
 | Tpat_record (l, cf) ->
    Pat_record (List.map (fun (lil, ld, p) -> (lil, ld, ml2fml_pattern p)) l, cf)
 | Tpat_tuple l -> Pat_tuple (List.map ml2fml_pattern l)
 | Tpat_alias _ -> out_of_scope Location.none "alias in let"
 | Tpat_variant _ -> out_of_scope Location.none "variant in let"
 | Tpat_array _ -> out_of_scope Location.none "array in let"
 | Tpat_or _ -> out_of_scope Location.none "or in let"
 | Tpat_lazy _ -> out_of_scope Location.none "lazy"


and ml2fml_exp_option = function
 | None -> None
 | Some e -> Some (ml2fml_exp e)

and ml2fml_record_label_definition = function
 | Typedtree.Kept te -> Kept te
 | Typedtree.Overridden (i, e) -> Overridden (i, ml2fml_exp e)

and ml2fml_case { c_lhs; c_guard; c_rhs } =
 { c_lhs = ml2fml_pattern c_lhs;
   c_guard = ml2fml_exp_option c_guard;
   c_rhs = ml2fml_exp c_rhs
 }

and ml2fml_structure_item (si : Typedtree.structure_item) =
 { fstr_desc = ml2fml_structure_item_desc si.str_desc;
   fstr_loc = si.str_loc;
   fstr_env = si.str_env }

and ml2fml_structure_item_desc = 
  let loc = Location.none in
  function
  | Tstr_eval (e, _) -> Fml_tstr_eval (ml2fml_exp e)
  | Tstr_value (_, vb_l) -> Fml_tstr_value (List.map ml2fml_value_binding vb_l)
  | Tstr_type (_, td_l) -> Fml_tstr_type (List.map ml2fml_type_declaration td_l)
  | Tstr_attribute a -> Fml_attribute a
  | Tstr_open _ -> out_of_scope loc "open" (* TODO/FIXME : CHECKME  *)
  | Tstr_modtype _ -> out_of_scope loc "modtype" (* TODO/FIXME : CHECKME  *)
  | Tstr_module     _  -> out_of_scope loc "modules"
  | Tstr_primitive  _  -> out_of_scope loc "primitive functions"
  | Tstr_typext     _  -> out_of_scope loc "type extensions"
  | Tstr_exception  _  -> out_of_scope loc "exceptions"
  | Tstr_recmodule  _  -> out_of_scope loc "recursive modules"
  | Tstr_class      _  -> out_of_scope loc "objects"
  | Tstr_class_type _  -> out_of_scope loc "class types"
  | Tstr_include    _  -> out_of_scope loc "includes"

and ml2fml_type_declaration (td : Typedtree.type_declaration) : fml_type_declaration = 
  { ftyp_name = td.typ_name;
    ftyp_type = td.typ_type;
  }

and ml2fml_type_kind = function
  | Ttype_variant cd_l -> Fml_t_variant cd_l
  | _ -> let loc = Location.none in out_of_scope loc "type_kind"

and ml2fml_structure (s : Typedtree.structure) : fml_structure = 
  { fstr_items = List.map ml2fml_structure_item s.str_items; 
    fstr_type = s.str_type;
    fstr_final_env = s.str_final_env;
  }
