val option_map : ('a -> 'b) -> 'a option -> 'b option
val option_iter : ('a -> unit) -> 'a option -> unit
val unsome : 'a option -> 'a
val option_to_list : 'a option -> 'a list
val option_app : 'a -> ('b -> 'a) -> 'b option -> 'a
val unsome_safe : 'a -> 'a option -> 'a
val bool_of_option : bool option -> bool
val list_make : int -> 'a -> 'a list
val list_mapi : 'a -> 'b -> (int -> 'c -> 'd) -> 'c list -> 'd list
val range : int -> int -> int list
val list_nat : int -> int list
val list_split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val list_separ : 'a -> 'a list -> 'a list
val filter_somes : 'a option list -> 'a list
val map_opt : ('a -> 'b option) -> 'a list -> 'b list
val map_opt2 : ('a -> 'b -> 'c option) -> 'a list -> 'b list -> 'c list
val map_state : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list
val map_opt_state :
  ('a -> 'b -> ('a * 'c) option) -> 'a -> 'b list -> 'a * 'c list
val map_opt_state2 :
  ('a -> 'b -> 'c -> ('a * 'd) option) ->
  'a -> 'b list -> 'c list -> 'a * 'd list
val list_unique : 'a list -> 'a list
val list_intersect : 'a list -> 'a list -> 'a list
val list_minus : 'a list -> 'a list -> 'a list
val list_concat_map : ('a -> 'b list) -> 'a list -> 'b list
val list_assoc_option : 'a -> ('a * 'b) list -> 'b option
val assoc_list_map : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
val list_remove : int -> 'a list -> 'a list
val list_replace : int -> 'a -> 'a list -> 'a list
val list_replace_nth : int -> 'a list -> 'a list -> 'a list
val list_ksort : ('a -> 'a -> int) -> ('a * 'b) list -> ('a * 'b) list
val list_index : 'a -> 'a list -> int
val add_to_list : 'a list ref -> 'a -> unit
val str_cmp : string -> string -> int
val str_starts_with : string -> string -> bool
val str_replace : char -> char -> string -> string
val cutlines : int -> string -> string
val make_upper : string -> string
val make_upper_2 : string -> string
val file_put_contents : string -> string -> unit
val output_endline : out_channel -> string -> unit
val gives_not_found : (unit -> 'a) -> bool
val lin0 : string
val lin1 : string
val lin2 : string
val show_list : ('a -> string) -> string -> 'a list -> string
val show_listp : ('a -> string) -> string -> 'a list -> string
val show_listq : ('a -> string) -> string -> 'a list -> string
val show_option : ('a -> string) -> 'a option -> string
val show_str : 'a -> 'a
val show_par : bool -> string -> string
val output : string -> unit
val unsupported : ?loc:Location.t -> string -> 'a
val out_of_scope : Location.t -> string -> 'a
val error : ?loc:Location.t -> string -> 'a
val warning : ?loc:Location.t -> string -> unit
