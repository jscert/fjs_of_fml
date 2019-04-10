exception Not_a_Tconstr
val get_type_name : Types.type_expr -> string
val test_type_name : string list -> Types.type_expr -> bool
val is_triple_equal_type : Types.type_expr -> bool
val is_infix : Fml.fml_expression -> Fml.fml_expression list -> bool
val fml_function_get_args_and_body :
  Fml.fml_expression -> Fml.fml_pattern list * Fml.fml_expression
val fml_exp_type_is_constant : Fml.fml_expression_desc -> bool
val function_get_args_and_body :
  Fml.fml_expression -> Fml.fml_pattern list * Fml.fml_expression
val is_ident : Fml.fml_expression -> bool
val is_hidden_type : Types.type_expr -> bool
val is_hidden_arg : Fml.fml_expression -> bool
val coercion_functions : string list
val is_function_without_event : Fml.fml_expression -> bool
val is_coercion_function : Fml.fml_expression -> bool
val is_coercion_constructor : Longident.t -> bool
val is_triple_equal_comparison :
  Fml.fml_expression -> int Ppf_helpers.ShadowMapM.t -> bool
val ppf_ident_of_pat :
  int Ppf_helpers.ShadowMapM.t -> Fml.fml_pattern -> string
val tuple_component_bind :
  string ->
  Fml.fml_pattern ->
  (string * string) list * int * int Ppf_helpers.ShadowMapM.t ->
  (string * string) list * int * int Ppf_helpers.ShadowMapM.t
val tuple_binders :
  string ->
  int Ppf_helpers.ShadowMapM.t ->
  Fml.fml_pattern list ->
  (string * string) list * int Ppf_helpers.ShadowMapM.t
val ocaml_doc_tags : string list
val is_doc_attr : Typedtree.attribute -> bool
val is_doc_texpr : Fml.fml_expression -> bool
val js_of_pattern :
  Ppf_helpers.shadow_map ->
  Fml.fml_pattern ->
  string -> string * (string * string) list * Ppf_helpers.shadow_map
val tag_url : string
val tag_id : string
val comment_of_value_binding :
  Fml.fml_value_binding -> string * string option
val js_of_fml_expression :
  Ppf_helpers.shadow_map ->
  string -> Ppf_helpers.dest -> string option -> Fml.fml_expression -> string
val js_of_fml_exp_naming_argument_if_non_variable :
  Ppf_helpers.shadow_map ->
  string -> Fml.fml_expression -> string option -> string -> string * string
val js_of_branch :
  Ppf_helpers.shadow_map ->
  string ->
  Ppf_helpers.dest -> Fml.fml_case -> string option -> string -> string
val js_of_fml_exp_inline_or_wrap :
  Ppf_helpers.shadow_map ->
  string -> string option -> Fml.fml_expression -> string
val js_of_fml_expression_wrapped :
  Ppf_helpers.shadow_map ->
  string -> string option -> Fml.fml_expression -> string
val js_of_let_pattern :
  Ppf_helpers.shadow_map ->
  Ppf_helpers.shadow_map ->
  string ->
  Fml.fml_value_binding ->
  string option ->
  Asttypes.rec_flag -> string * string * Ppf_helpers.shadow_map
val js_of_fml_structure_item :
  Fml.fml_structure_item -> string * string list * string option
exception Found_Url of string
val url : Fml.fml_structure_item list -> string
val js_of_fml_structure :
  Fml.fml_structure -> 'a -> string * string list * string
val to_javascript :
  string -> string -> Typedtree.structure -> string * string
