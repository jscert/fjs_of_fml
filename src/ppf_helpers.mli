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
val ppf_while : string -> string -> string
val ppf_for :
  string -> string -> string -> Asttypes.direction_flag -> string -> string
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
  Location.t -> string list -> string -> string -> string option -> string -> string
val generate_logged_return : Location.t -> string -> string -> string
