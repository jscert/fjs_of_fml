open Typedtree
open Attributes
open Mytools
open Fml


(**
 * Convert typedtree to fml AST
 *
 *)
let rec fml_of_structure typedtree =
  let rec extract_opens acc items =
    match items with
    | { str_desc = Fml_tstr_open od }::items2 ->
      extract_opens (od.open_path::acc) items2
    | _ -> (List.rev acc, items)
  in
  let open_paths, fml_of_struct_items = extract_opens [] (List.map ml2fml_structure_item typedtree.str_items) in
  (*let url_str = url items in*)
  fml_of_struct_items
  (*let contents, namesbound = combine_list_output  fml_of_struct_items in*)
  (* TODO/FIXME : dev context 
  let contents, namesbound = "", [] in

  let prefix = 
    List.fold_left (fun str path -> str ^ "with (" ^ Path.name path ^ ") {@,") "" open_paths in
  let postfix = 
    List.fold_left (fun str path -> str ^ "@,}// end of with " ^ Path.name path) "" open_paths in
  (prefix ^ "@," ^ contents ^ postfix, namesbound, url_str)*)



(**
 * Convert typedtree to string of javascript
 *
 *)
and to_javascript basename module_name typedtree = 
let _ = fml_of_structure typedtree in
"", ""
