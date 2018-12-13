val ( ~~ ) : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
module XBase : sig exception Break end
module XList : sig val rev_not_rec : 'a list -> 'a list end
module XFile :
  sig
    val put_contents : string -> string -> unit
    val put_lines : string -> ?sep:string -> string list -> unit
    exception FileNotFound of string
    val get_lines : string -> string list
  end
val hashtbl_keys : ('a, 'b) Hashtbl.t -> 'a list
type pos = { pos_line : int; pos_col : int; }
type tokens_start = (int, pos) Hashtbl.t
type tokens_stop = (int, pos) Hashtbl.t
type tokens = (string * tokens_start * tokens_stop) list ref
val tokens : tokens
val gather_tokens : string -> string list -> unit
val generate_lineof_entries : (string -> unit) -> unit
val files : string list ref
val outputfile : string option ref
