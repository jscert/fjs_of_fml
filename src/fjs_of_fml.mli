val ppf : Format.formatter
val stdlib_path : string ref
val tool_name : string
val init_path : unit -> unit
val initial_env : unit -> Env.t
val process_implementation_file :
  Format.formatter ->
  string ->
  string ->
  Parsetree.structure * (Typedtree.structure * Typedtree.module_coercion) *
  string
