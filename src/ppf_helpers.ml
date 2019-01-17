open Mytools
open Log
open Asttypes
open Params

module L = Logged (Token_generator) (struct let size = 256 end)

(****************************************************************)
(* PPF HELPERS *)

(**
 * Before-hand definitions of Pretty-Printer-Format for converting ocaml
 * to ECMAScript, therefore all of them are in a single place.
 *)

let ppf_lambda_wrap s =
  Printf.sprintf "(function () {@;<1 2>@[<v 0>%s@]@,}())@," s

let ppf_function args body=
  (L.log_line (Printf.sprintf "function (%s) {" args) [L.Enter; (L.CreateCtx args)]) ^ (Printf.sprintf "@;<1 2>return@[<hov 2>@ (%s);@]@,}" body)

let ppf_apply f args =
  Printf.sprintf "@[<hov 2>%s(@,%s)@]"
                 f args

let ppf_apply_infix f arg1 arg2 =
  Printf.sprintf "@[<hov 0>(%s@ %s %s)@]"
                 arg1 f arg2
  (* todo: only put parentheses if needed *)

let ppf_match_case c =
  Printf.sprintf "case %s" c

(* FIXME: shadows now a sm, always should introduce a var *)
let ppf_match_binders binders =
  if binders = [] then "" else
    let binds = show_list "@," (List.map
      (fun (id,se) -> Printf.sprintf "var %s = %s;" id se) binders) in
  Printf.sprintf "@[<hov 2>%s@]" binds

let ppf_let_tuple ids sbody =
  assert (ids <> []);
  Printf.sprintf "@[<hov 2>var (%s) = %s;@]" (show_list ",@ " ids) sbody

let ppf_let_record ids sbody =
  Printf.sprintf "@[<hov 2>var {%s} = %s;@]" (show_list ",@ " ids) sbody

let ppf_array values =
  Printf.sprintf "[%s]"
                 values

let ppf_tuple = ppf_array

let ppf_sequence exp1 exp2 =
  Printf.sprintf "%s;@,%s"
                 exp1 exp2

let ppf_while cd body =
  let s =
    Printf.sprintf "@[<v 2>while(%s) {@;<1 2>%s@,@]}"
                   cd body
  in ppf_lambda_wrap s

let ppf_for id start ed flag body =
  let fl_to_string = function
    | Upto   -> "++"
    | Downto -> "--" in
  let fl_to_symbl = function
    | Upto   -> "<="
    | Downto -> ">=" in
  let s =
    Printf.sprintf "@[<v 2>for (%s = %s ; %s %s %s ; %s%s) {@,%s@]@,}"
                   id start id (fl_to_symbl flag) ed (fl_to_string flag) id body
  in ppf_lambda_wrap s

(*let ppf_single_cstr tag =
  Printf.sprintf "%s"
    tag
*)
let ppf_cstr tag value =
  Some (Printf.sprintf "%s: %s" tag value)

let ppf_cstrs styp cstr_name rest current_mode =
  let comma = if rest = "" then "" else "," in
  let styp_full =
    match current_mode with
    | Mode_cmi -> assert false
    | Mode_unlogged _ | Mode_pseudo _ -> ""
    | Mode_logged -> Printf.sprintf "type: \"%s\", " styp
    in
  Printf.sprintf "{@[<v 2>%stag: \"%s\"%s %s@]}" (* TODO: cleanup *)
    styp_full cstr_name comma rest

let ppf_cstrs_fct cstr_fullname args =
  if is_mode_pseudo() && args = [] 
    then cstr_fullname 
    else ppf_apply cstr_fullname (show_list ",@ " args)

let ppf_record llde =
  let rec aux acc = function
    | []               -> Printf.sprintf "{@;<1 2>@[<v 0>%s@]@,}" (*"@[<v 2>{@;<1 2>%s@]@,}"*) (* TODO: cleanup *) acc
    | (lbl, exp) :: [] -> aux (acc ^ Printf.sprintf "%s: %s" lbl exp) []
    | (lbl, exp) :: xs -> aux (acc ^ Printf.sprintf "%s: %s,@," lbl exp) xs
  in aux "" llde

let ppf_record_with seinit assign_exp =
  ppf_apply "Object.assign" (show_list ",@ " [ "{}"; seinit; assign_exp ])

let ppf_decl id expr = Printf.sprintf "@[<v 0>%s: %s,@,@]" id expr

let ppf_pat_array id_list array_expr =
  Printf.sprintf "var __%s = %s;@," "array" array_expr ^
    List.fold_left2 (fun acc (name, exp_type) y -> acc ^ Printf.sprintf "@[<v 0>var %s = __%s[%d];@,@]" name "array" y)
                    "" id_list @@ range 0 (List.length id_list - 1)

let ppf_field_access expr field =
  Printf.sprintf "%s.%s" expr field

let ppf_comment c = Printf.sprintf "@[<v 0>/*@,%s@,*/@]" c

let ppf_value id body comment = 
  if comment = "" then Printf.sprintf "@[<v 0>var %s = %s;@]" id body else
  Printf.sprintf "@[<v 0>/*@,%s@,*/@,var %s = %s;@]" comment id body

let ppf_path =
  Path.name

let ppf_module content =
  Printf.sprintf "{@,%s@,}" content

let ppf_module_wrap name content names_bound =
  let bindings = show_list ", " (List.map (fun id -> Printf.sprintf "@;<0 2>%s: %s" id id) names_bound) in
  Printf.sprintf "@[<v 0>var %s = (function() {@,%s@,@,return {%s};@,})();@,@]" name content bindings

(****************************************************************)
(* FRESH ID NAMES *)

let id_fresh =
  let r = ref 0 in
  fun prefix -> (incr r; prefix ^ string_of_int !r) 

(****************************************************************)
(* TOKEN TO LOC BINDINGS FOR THE ML SOURCE FILES *)

(* Keeps track of the location associated with each token,
   maps int to (pos*pos).  *)

type pos = { pos_line: int; pos_col: int }
let token_locs = Hashtbl.create 50 

let pos_of_lexing_pos lexing_pos =
  let (file, line, char) = Location.get_pos_info lexing_pos in
  { pos_line = line; pos_col = char } 

let pos_pair_of_loc loc =
  (pos_of_lexing_pos loc.Location.loc_start,
   pos_of_lexing_pos loc.Location.loc_end)

(****************************************************************)
(* FRESH TOKEN NAMES *)

let token_basename_ref = ref "no_token_basename_registered"

let token_register_basename basename =
  token_basename_ref := basename

(* returns a string of the form: ["filename.js", 3425],
   where 3425 describes the token. *)

let token_fresh =
  let r = ref 0 in
  fun mode loc -> (
    incr r; 
    Hashtbl.add token_locs (!r) (pos_pair_of_loc loc);
    (* if mode = Mode_unlogged TokenFalse then ("", "", "") else begin end*)
    let token_start = Printf.sprintf "@{<%d>" !r in
    let token_stop = "@}" in
    let token_loc = Printf.sprintf "\"%s.js\", %d" !token_basename_ref !r in 
    (token_start, token_stop, token_loc)
    )


(****************************************************************)
(* LOGGED CONSTRUCTORS *)




(*--------- if ---------*)

let ppf_ifthenelse arg iftrue iffalse =
  Printf.sprintf "@[<v 0>if (%s) {@;<1 2>@[<v 0>%s@]@,} else {@;<1 2>@[<hv 0>%s@]@,}@]"
                 arg iftrue iffalse

let generate_logged_if loc ctx sintro sarg siftrue siffalse =
  (* sintro is not empty only in the logged case,
     it describes the binding of the value describing the argument of the if *)
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ ->
     let sarg_with_token = Printf.sprintf "%s%s%s" token_start sarg token_stop in
     ppf_ifthenelse sarg_with_token siftrue siffalse
  | Mode_logged ->
     let sevent = Printf.sprintf "%slog_event(%s, %s, \"if\");@,"
        sintro token_loc ctx in
     let sbody = ppf_ifthenelse sarg siftrue siffalse in
     sevent ^ sbody


  (* TODO: extend the ctx with if_arg *)

(*--------- match ---------*)

let generate_logged_case loc spat binders ctx newctx sbody need_break =
  (* Note: if binders = [], then newctx = ctx *)
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  let sbinders_common () = 
    Printf.sprintf "%s%s" (if binders = [] then "" else "@;<1 2>") (ppf_match_binders binders) in
  let (shead, spat, sbinders, sintro) =
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_pseudo _ ->
        let args = List.map fst binders in
        let spat = (* LATER: use a cleaner separation with Case of (cstr,args) | Default *)
          if spat = "case ::" then begin
            let (x,y) = match args with [x;y] -> (x,y) | _ -> assert false in
            Printf.sprintf "case (%s::%s)" x y
          end else if args = [] then begin
            spat 
          end else begin
            ppf_apply spat (show_list ",@ " args)
          end in
        (token_start, spat, "", token_stop)
    | Mode_unlogged _ ->
        (token_start, spat, sbinders_common(), token_stop)
    | Mode_logged ->
      let ids = List.map fst binders in
      let mk_binding x =
        Printf.sprintf "{key: \"%s\", val: %s}" x x
      in
      let bindings =
        Printf.sprintf "[%s]" (show_list ", " (List.map mk_binding ids))
      in 
      let spreintro =
        if binders = [] then ""
        else Printf.sprintf "var %s = ctx_push(%s, %s);@," newctx ctx bindings
      in
      let sintro = Printf.sprintf "%slog_event(%s, %s, \"case\");@,"
        spreintro token_loc newctx in
      ("", spat, sbinders_common(), sintro)
    in
  (Printf.sprintf "@[<v 0>%s%s:%s%s@;<1 2>@[<v 0>%s%s@]@]"
     shead spat sbinders sintro sbody
     (if need_break then "@,break;" else ""))

let ppf_match sintro sarg sbranches =
  let sswitch, sbranches = 
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_pseudo _ -> (*"match"*) "switch", sbranches
    | Mode_unlogged _ -> "switch", sbranches
    | Mode_logged -> "switch", sbranches 
      (* TODO: put back if there is not already a default case:
          ^ "@,default: throw \"No matching case for switch\";" *)
    in
  Printf.sprintf "%s%s (%s) {@;<1 2>@[<v 0>%s@]@,}@,"
    sintro sswitch sarg sbranches

let generate_logged_match loc ctx sintro sarg sbranches arg_is_constant =
  (* sintro is useful not just in the logged case, but also in unlogged;
     this is needed for the semantics *)
  (* arg_is_constant describes whether the argument of switch is a basic JS value,
     or whether it is an encoded object from which we need to read the tag field *)
  let sarg = if arg_is_constant || is_mode_pseudo() then sarg else sarg ^ ".tag" in
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ ->
     let sarg_with_token = Printf.sprintf "%s%s%s" token_start sarg token_stop in
     ppf_match sintro sarg_with_token sbranches 
  | Mode_logged ->
     let sbody = ppf_match "" sarg sbranches in
     Printf.sprintf "%slog_event(%s, %s, \"switch\");@,%s"
        sintro token_loc ctx sbody

  (* TODO: extend the ctx with switch_arg *)

(*--------- let ---------*)

let generate_logged_let loc ids ctx newctx sdecl sbody =
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ -> 
     Printf.sprintf "%s%s%s@,%s" token_start sdecl token_stop sbody  
  | Mode_logged ->
    let mk_binding x =
      Printf.sprintf "{key: \"%s\", val: %s}" x x in
    let bindings =
      Printf.sprintf "[%s]" (show_list ", " (List.map mk_binding ids)) in 
    Printf.sprintf "%s@,var %s = ctx_push(%s, %s);@,log_event(%s, %s, \"let\");@,%s@,"
      sdecl newctx ctx bindings token_loc newctx sbody


(*--------- function call ---------*)

let generate_logged_apply loc ctx sbody =
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ ->
     Printf.sprintf "%s%s%s" token_start sbody token_stop
  | Mode_logged ->
     Printf.sprintf "log_event(%s, %s, \"call\");@,%s" token_loc ctx sbody


(*--------- enter function body ---------*)

let generate_logged_enter loc arg_ids ctx newctx sbody = 
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  let (shead1, shead2, sintro) =
    match !current_mode with
    | Mode_cmi -> assert false
    | Mode_unlogged _ | Mode_pseudo _ -> (token_start, token_stop, "")
    | Mode_logged ->
      let mk_binding x =
        Printf.sprintf "{key: \"%s\", val: %s, id: %s}" x x x
      in
      let bindings =
        Printf.sprintf "[%s]" (show_list ", " (List.map mk_binding arg_ids))
      in 
      let sintro = Printf.sprintf "var %s = ctx_push(%s, %s);@,log_event(%s, %s, \"enter\");@,"
        newctx ctx bindings token_loc newctx in
      ("", "", sintro)
  in
  let args = String.concat ", " arg_ids in
  Printf.sprintf "%sfunction (%s)%s {@;<1 2>@[<v 0>%s%s@]@,}" shead1 args shead2 sintro sbody


(*--------- return ---------*)

(* possibly: optimize return when it's a value *)

let generate_logged_return loc ctx sbody = 
  let (token_start, token_stop, token_loc) = token_fresh !current_mode loc in
  match !current_mode with
  | Mode_cmi -> assert false
  | Mode_unlogged _ | Mode_pseudo _ ->
     Printf.sprintf "@[<hv 2>%sreturn (@,%s);%s@]" token_start sbody token_stop
  | Mode_logged ->
    let id = id_fresh "_return_" in
    Printf.sprintf "var %s = %s;@,log_event(%s, ctx_push(%s, [{key: \"#RETURN_VALUE#\", val: %s}]), \"return\");@,return (%s); "
      id sbody token_loc ctx id id
