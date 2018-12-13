val builtin_attributes : (string * string list) list
val ident_builtin_attributes : (Ident.t * string list) list
val extract_attrs :
  (string Asttypes.loc * Parsetree.payload) list -> string list
val extract_attr : string Asttypes.loc * Parsetree.payload -> string list
val extract_payload : Parsetree.payload -> string list
val extract_structure : Parsetree.structure -> string list
val extract_structure_item : Parsetree.structure_item -> string list
val extract_expression : Parsetree.expression -> string list
val extract_constant : Parsetree.constant -> string
val fetch_builtin_attrs : string -> string list
val extract_cstr_attrs_basic :
  string -> (string Asttypes.loc * Parsetree.payload) list -> string list
val extract_cstr_attrs : Types.constructor_description -> string list
