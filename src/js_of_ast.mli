val is_infix : Typedtree.expression -> Typedtree.expression list -> bool
val function_get_args_and_body :
  Typedtree.expression -> Typedtree.pattern list * Typedtree.expression
exception Not_a_Tconstr
val get_type_name : Types.type_expr -> string
val test_type_name : string list -> Types.type_expr -> bool
val is_triple_equal_type : Types.type_expr -> bool
val is_ident : Typedtree.expression -> bool
val is_hidden_type : Types.type_expr -> bool
val is_hidden_arg : Typedtree.expression -> bool
val coercion_functions : string list
val is_function_without_event : Typedtree.expression -> bool
val is_coercion_function : Typedtree.expression -> bool
val is_coercion_constructor : Longident.t -> bool
val is_triple_equal_comparison :
  Typedtree.expression -> int Ppf_helpers.ShadowMapM.t -> bool
val ppf_ident_of_pat :
  int Ppf_helpers.ShadowMapM.t -> Typedtree.pattern -> string
val tuple_component_bind :
  string ->
  Typedtree.pattern ->
  (string * string) list * int * int Ppf_helpers.ShadowMapM.t ->
  (string * string) list * int * int Ppf_helpers.ShadowMapM.t
val tuple_binders :
  string ->
  int Ppf_helpers.ShadowMapM.t ->
  Typedtree.pattern list ->
  (string * string) list * int Ppf_helpers.ShadowMapM.t
val ocaml_doc_tags : string list
val is_doc_attr : string Asttypes.loc * 'a -> bool
val is_doc_texpr : Typedtree.expression -> bool
val tag_url : string
val tag_id : string
exception Found_Url of string
val url : Typedtree.structure_item list -> string
val js_of_structure : Typedtree.structure -> string * string list * string
val js_of_structure_item :
  Typedtree.structure_item -> string * string list * string option
val js_of_branch :
  Ppf_helpers.shadow_map ->
  string ->
  Ppf_helpers.dest -> Typedtree.case -> string option -> string -> string
val js_of_expression_inline_or_wrap :
  int Ppf_helpers.ShadowMapM.t ->
  string -> string option -> Typedtree.expression -> string
val js_of_expression_wrapped :
  int Ppf_helpers.ShadowMapM.t ->
  string -> string option -> Typedtree.expression -> string
val js_of_expression_naming_argument_if_non_variable :
  int Ppf_helpers.ShadowMapM.t ->
  string ->
  Typedtree.expression -> string option -> string -> string * string
val js_of_expression :
  int Ppf_helpers.ShadowMapM.t ->
  string ->
  Ppf_helpers.dest -> string option -> Typedtree.expression -> string
val js_of_let_pattern :
  Ppf_helpers.shadow_map ->
  Ppf_helpers.shadow_map ->
  string ->
  Typedtree.value_binding ->
  string option ->
  Asttypes.rec_flag -> string * string * Ppf_helpers.shadow_map
val js_of_pattern :
  Ppf_helpers.shadow_map ->
  Typedtree.pattern ->
  string -> string * (string * string) list * int Ppf_helpers.ShadowMapM.t
val to_javascript :
  string -> string -> Typedtree.structure -> string * string
