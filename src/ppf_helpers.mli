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
val ppf_cstr : string -> string -> string option
val ppf_cstrs : string -> string -> string -> Params.generate_mode -> string
val ppf_cstrs_fct : string -> string list -> string
val ppf_record : (string * string) list -> string
val ppf_record_with : string -> string -> string
val ppf_decl : string -> string -> string
val ppf_pat_array : (string * 'a) list -> string -> string
val ppf_field_access : string -> string -> string
val ppf_comment : string -> string
val ppf_value : string -> string -> string -> string
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
  Location.t ->
  string list -> string -> string -> string option -> string -> string
val generate_logged_return : Location.t -> string -> string -> string
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
type dest =
    Dest_ignore
  | Dest_return
  | Dest_assign of string * bool
  | Dest_inline
val apply_dest : Location.t -> string -> dest -> string -> string
exception Not_good_for_dest_inline
val reject_inline : dest -> unit
val is_sbool : string -> bool
val is_unit : string -> bool
val unit_repr : string
val map_cstr_fields :
  ?loc:Location.t ->
  shadow_map ->
  (shadow_map -> string -> 'a -> (shadow_map * 'b) option) ->
  Types.constructor_description -> 'a list -> shadow_map * 'b list
val js_keywords : string list
val ustr_bb_digits : string array
val int_to_array : int -> int list
val int_to_bb_ustr : int -> string
val ppf_ident_name : ShadowMapM.key -> int ShadowMapM.t -> string
val ppf_ident : Ident.t -> int ShadowMapM.t -> string
val js_of_constant : Asttypes.constant -> string
val js_of_path_longident :
  int ShadowMapM.t -> Path.t -> Longident.t Asttypes.loc -> string
val ctx_fresh : unit -> string
val ctx_initial : string
