val mk_lid : ?loc:Location.t -> string -> Longident.t Location.loc
val mk_ident : ?loc:Location.t -> string -> Parsetree.expression
val generate_mapper : (string * string) list -> 'a -> Ast_mapper.mapper
