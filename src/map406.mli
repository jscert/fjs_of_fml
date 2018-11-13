module Make :
  functor (Ord : Map.OrderedType) ->
    sig
      type key = Ord.t
      type 'a t = 'a Map.Make(Ord).t
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
      val find_opt : Ord.t -> 'a t -> 'a option
    end
