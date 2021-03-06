open Params
open Format
open Mytools
open Ppf_helpers

(*
   Remark: field name attributes for builtins (eg: ::) are defined in attributes.ml
   Remark: field name attributes should not be "type" or "tag".
*)


(*#########################################################################*)

let ppf = Format.std_formatter
let stdlib_path = ref "stdlib_fml"

(* err_formatter *)


(*#########################################################################*)


let tool_name = "ml2js"

(** Configures the compilers load paths from the commandline and stdlib *)
(* FIXME: Relative stdlib dir should be absolute or findlib derived
 * we're manually specified using -I for now... *)
let init_path () =
  (* Compmisc.init_path false; (* to use this, lots of parameters need tweaking *)*)
  Config.load_path := "" :: List.rev_append !Clflags.include_dirs [!stdlib_path];
  Env.reset_cache ()

(** Return the initial environment in which compilation proceeds. *)
let initial_env () =
  Clflags.nopervasives := true;
  (* Stdlib module name, instead of Pervasives *)
  add_to_list Compenv.implicit_modules "Stdlib_fml";
  Compmisc.initial_env ()


(** Analysis of an implementation file.
 *  ppf: error printer
 *  sourcefile: path/filename of source file
 *  oprefix: output file name prefix (possibly manually set with -o)
 **)
let process_implementation_file ppf sourcefile oprefix =
  init_path ();
  let modulename = Compenv.module_of_filename ppf sourcefile oprefix in
  Env.set_unit_name modulename;
  try
    let env = initial_env () in
    let parsetree = Pparse.parse_implementation ~tool_name ppf sourcefile in
    if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.structure parsetree;
    let typing = Typemod.type_implementation sourcefile oprefix modulename env parsetree in
    (parsetree, typing, modulename)
  with e ->
    Location.report_exception ppf e;
    exit 2

let _ =
  (*---------------------------------------------------*)
  (* parsing of command line *)

  let files = ref [] in
  Arg.parse
    [ ("-stdlib", Arg.Set_string stdlib_path, "path to look for Stdlib (defaults to 'stdlib_fml')");
      ("-I", Arg.String (add_to_list Clflags.include_dirs), "includes a directory where to look for interface files");
      ("-o", Arg.String (fun s -> Clflags.output_name := Some s), "set the output file");
      ("-debug", Arg.Set debug, "trace the various steps");
      ("-dsource", Arg.Set Clflags.dump_source, "dump source after ppx");
      ("-ppx", Arg.String (add_to_list Clflags.all_ppx (* TODO Compenv.first_ppx *) ), "load ppx");
      ("-mode", Arg.String (fun s -> set_current_mode s), "current mode: unlog, log, or token");
      ("-dev", Arg.Set dev, "js_of_ast2");
    ]
    (fun f -> files := f :: !files)
    ("usage: [-I dir] [..other options..] file.ml");


  files := List.rev !files;
  if List.length !files <> 1 then
    failwith "Expects one argument: the filename of the ML source file";
  let sourcefile = List.hd !files in

  if not (Filename.check_suffix sourcefile ".ml") then
    failwith "The file name must be of the form *.ml";

  let sourcebase  = Filename.basename sourcefile in       (* Input file basename, for logging *)
  let prefix_file = Filename.chop_extension sourcebase in (* File basename, no extension *)
  let prefix_path = Filename.chop_extension sourcefile in (* Output file path, no extension *)

  let output_file  = prefix_path ^ (get_mode_extension !current_mode) in
  let mlloc_output = prefix_path ^ ".mlloc.js" in

  let url_output = prefix_path ^ ".url.js" in

  (*---------------------------------------------------*)
  (* set flags *)

  if !current_mode <> Mode_cmi
  then Clflags.dont_write_files := true;

  (* TODO: does not work because we don't have easy access to the fully qualified path of constructors
     if !current_mode <> Mode_unlogged 
     then generate_qualified_names := true;
  *)


  (*---------------------------------------------------*)
  (* generation of the mlloc file that binds tokens to positions *)

  let generate_mlloc_file () =
    let outchannel = open_out mlloc_output in
    let put = output_endline outchannel in
    put "   lineof_temp = [];";
    Js_of_fml.(
      ~~ Hashtbl.iter token_locs (fun key (pos_start,pos_stop) ->
          put (Printf.sprintf "   lineof_temp[%d] = [%d,%d,%d,%d];" 
                 key pos_start.pos_line pos_start.pos_col  
                 pos_stop.pos_line  pos_stop.pos_col);
        ));
    put (Printf.sprintf "lineof_data[\"%s\"] = lineof_temp;" sourcebase);
    close_out outchannel;
    Printf.printf "Wrote %s\n" mlloc_output;
  in

  (*---------------------------------------------------*)
  (* "reading and typing source file" *)

  let (parsetree, (typedtree,_), module_name) = process_implementation_file ppf sourcefile prefix_path in
  if !current_mode <> Mode_cmi then begin
    let out, url_str = Js_of_fml.to_javascript prefix_file module_name typedtree in
    file_put_contents output_file out;
    if url_str <> "" then file_put_contents url_output ("const url_root = \"" ^ url_str ^ "\";") else ();
    if !current_mode = (Mode_unlogged TokenTrue) 
    then generate_mlloc_file()
  end;
  Printf.printf "Wrote %s\n" output_file 
