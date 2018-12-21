val string_of_longident : Longident.t -> string
module ShadowMapM :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type shadow_map = int ShadowMapM.t
val increment_sm : int ShadowMapM.t -> ShadowMapM.key -> int ShadowMapM.t
val ident_is_shadowing : Env.t -> string -> bool
val update_shadow_map :
  int ShadowMapM.t -> Env.t -> Ident.t -> int ShadowMapM.t
val is_sbool : string -> bool
val is_unit : string -> bool
val unit_repr : string
val exp_type_is_constant : Typedtree.expression -> bool
val is_infix : Typedtree.expression -> Typedtree.expression list -> bool
val map_cstr_fields :
  ?loc:Location.t ->
  shadow_map ->
  (shadow_map -> string -> 'a -> (shadow_map * 'b) option) ->
  Types.constructor_description -> 'a list -> shadow_map * 'b list
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
val js_keywords : string list
val ustr_bb_digits : string array
val int_to_array : int -> int list
val int_to_bb_ustr : int -> string
val ppf_ident_name : ShadowMapM.key -> int ShadowMapM.t -> string
val ppf_ident : Ident.t -> int ShadowMapM.t -> string
val ctx_fresh : unit -> string
val ctx_initial : string
type dest =
    Dest_ignore
  | Dest_return
  | Dest_assign of string * bool
  | Dest_inline
val apply_dest : Location.t -> string -> dest -> string -> string
exception Not_good_for_dest_inline
val reject_inline : dest -> unit
val js_of_constant : Asttypes.constant -> string
val js_of_path_longident :
  int ShadowMapM.t -> Path.t -> Longident.t Asttypes.loc -> string
val is_triple_equal_comparison :
  Typedtree.expression -> int ShadowMapM.t -> bool
val ppf_ident_of_pat : int ShadowMapM.t -> Typedtree.pattern -> string
val combine_list_output : (string * 'a list) list -> string * 'a list
val tuple_component_bind :
  string ->
  Typedtree.pattern ->
  (string * string) list * int * int ShadowMapM.t ->
  (string * string) list * int * int ShadowMapM.t
val tuple_binders :
  string ->
  int ShadowMapM.t ->
  Typedtree.pattern list -> (string * string) list * int ShadowMapM.t
val ocaml_doc_tags : string list
val is_doc_attr : string Asttypes.loc * 'a -> bool
val is_doc_texpr : Typedtree.expression -> bool
exception Found_Url of string
val url : Typedtree.structure_item list -> string
val set_url : string -> Typedtree.structure_item -> Typedtree.structure_item
val js_of_structure : Typedtree.structure -> string * string list
val js_of_structure_item : Typedtree.structure_item -> string * string list
val js_of_branch :
  shadow_map -> string -> dest -> Typedtree.case -> string -> string
val js_of_expression_inline_or_wrap :
  int ShadowMapM.t -> string -> Typedtree.expression -> string
val js_of_expression_wrapped :
  int ShadowMapM.t -> string -> Typedtree.expression -> string
val js_of_expression_naming_argument_if_non_variable :
  int ShadowMapM.t ->
  string -> Typedtree.expression -> string -> string * string
val js_of_expression :
  int ShadowMapM.t -> string -> dest -> Typedtree.expression -> string
val js_of_let_pattern :
  shadow_map ->
  shadow_map ->
  string ->
  Typedtree.value_binding ->
  Asttypes.rec_flag -> string * string * shadow_map
val js_of_pattern :
  shadow_map ->
  Typedtree.pattern ->
  string -> string * (string * string) list * int ShadowMapM.t
val to_javascript : string -> string -> Typedtree.structure -> string
