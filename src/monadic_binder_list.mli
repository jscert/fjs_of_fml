val monad_mapping : (string * string) list
val monad_identifiers : string list
val attr_tag : string
val monadic_expr : Parsetree.expression -> Parsetree.expression
val is_monadic_attr : string Asttypes.loc * 'a -> bool
val is_monadic_expr : Parsetree.expression -> bool
val is_monadic_texpr : Typedtree.expression -> bool
