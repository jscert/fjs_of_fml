module L :
  sig
    type token
    type token_info
    type ident = string
    type typ = string
    type func = string
    type ctx_operation =
        Add of ident * typ
      | CreateCtx of ident
      | ReturnStrip
      | Enter
      | Exit
    val log_line : string -> ctx_operation list -> string
    val strip_log_info : string -> string
    val logged_output : string -> string
    val unlogged_output : string -> string
  end
val string_of_longident : Longident.t -> string
module ShadowMapM :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
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
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    type 'a impl =
      'a Map406.Make(String).impl =
        Empty
      | Node of { l : 'a impl; v : key; d : 'a; r : 'a impl; h : int; }
    external impl_of_t : 'a t -> 'a impl = "%identity"
    external t_of_impl : 'a impl -> 'a t = "%identity"
    val height : 'a impl -> int
    val create : 'a impl -> key -> 'a -> 'a impl -> 'a impl
    val bal : 'a impl -> key -> 'a -> 'a impl -> 'a impl
    val remove_min_binding : 'a impl -> 'a impl
    val merge' : 'a impl -> 'a impl -> 'a impl
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val find_opt : String.t -> 'a t -> 'a option
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
val ppf_lambda_wrap : string -> string
val ppf_function : L.ident -> string -> string
val ppf_apply : string -> string -> string
val ppf_apply_infix : string -> string -> string -> string
val ppf_match_case : string -> string
val ppf_match_binders : (string * string) list -> string
val ppf_let_tuple : string list -> string -> string
val ppf_let_record : string list -> string -> string
val ppf_array : string -> string
val ppf_tuple : string -> string
val ppf_sequence : string -> string -> string
val ppf_while : string -> string -> string
val ppf_for :
  string -> string -> string -> Asttypes.direction_flag -> string -> string
val ppf_cstr : string -> string -> string option
val ppf_cstrs : string -> string -> string -> string
val ppf_cstrs_fct : string -> string list -> string
val ppf_record : (string * string) list -> string
val ppf_record_with : string -> string -> string
val ppf_decl : string -> string -> string
val ppf_pat_array : (string * 'a) list -> string -> string
val ppf_field_access : string -> string -> string
val ppf_comment : string -> string
val js_keywords : string list
val ustr_bb_digits : string array
val int_to_array : int -> int list
val int_to_bb_ustr : int -> string
val ppf_ident_name : String.t -> int ShadowMapM.t -> string
val ppf_ident : Ident.t -> int ShadowMapM.t -> string
val ppf_path : ?paren:(string -> bool) -> Path.t -> string
val ppf_module : string -> string
val ppf_module_wrap : string -> string -> string list -> string
val id_fresh : string -> string
type pos = { pos_line : int; pos_col : int; }
val token_locs : (int, pos * pos) Hashtbl.t
val pos_of_lexing_pos : Lexing.position -> pos
val pos_pair_of_loc : Location.t -> pos * pos
val token_basename_ref : string ref
val token_register_basename : string -> unit
val token_fresh :
  Params.generate_mode -> Location.t -> string * string * string
val ctx_fresh : unit -> string
val ctx_initial : string
val ppf_ifthenelse : string -> string -> string -> string
val generate_logged_if :
  Location.t -> string -> string -> string -> string -> string -> string
val generate_logged_case :
  Location.t ->
  string ->
  (string * string) list -> string -> string -> string -> bool -> string
val ppf_match : string -> string -> string -> string
val generate_logged_match :
  Location.t -> string -> string -> string -> string -> bool -> string
val generate_logged_let :
  Location.t -> string list -> string -> string -> string -> string -> string
val generate_logged_apply : Location.t -> string -> string -> string
val generate_logged_enter :
  Location.t -> string list -> string -> string -> string -> string
val generate_logged_return : Location.t -> string -> string -> string
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
