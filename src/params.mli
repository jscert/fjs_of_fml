val debug : bool ref
val ( ~~ ) : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
type generate_token_flag = TokenTrue | TokenFalse
type generate_mode =
    Mode_pseudo of generate_token_flag
  | Mode_unlogged of generate_token_flag
  | Mode_logged
  | Mode_cmi
val current_mode : generate_mode ref
val set_current_mode : string -> unit
val get_mode_extension : generate_mode -> string
val is_mode_pseudo : unit -> bool
val is_mode_not_pseudo : unit -> bool
val generate_qualified_names : bool ref
