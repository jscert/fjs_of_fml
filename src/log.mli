module Token_generator :
  sig
    type token
    val build : int -> token option
    val string_of_token : token -> string
    val token_of_string : string -> token
    val reset : unit -> unit
    val withdraw : unit -> token
  end
module Logged :
  functor
    (G : sig
           type token
           val build : int -> token option
           val string_of_token : token -> string
           val token_of_string : string -> token
           val reset : unit -> unit
           val withdraw : unit -> token
         end) (Sz : sig val size : int end) ->
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
