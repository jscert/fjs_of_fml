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
val files : string list ref
val outputfile : string option ref
