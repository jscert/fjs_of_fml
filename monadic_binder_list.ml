let monad_mapping = [
    ("string", "if_string");
    ("object", "if_object");
    ("OBJECT", "assert_object");
    ("value", "if_value");
    ("prim", "if_prim");
    ("number", "if_number");
    ("some", "if_some");
    ("bool", "if_bool");
    ("void", "if_void");
    ("success", "if_success");
    ("not_throw", "if_not_throw");
    ("ter", "if_ter");
    ("break", "if_break");
    ("spec", "if_spec");
    ("assert", "check_assert");

    ("ret", "let_ret");
    ("value_ret", "if_value_ret");
    ("object_ret", "if_object_ret");
    ("OBJECT_ret", "assert_object_ret");
    ("assert_ret", "check_assert_ret");
   ]

let monad_identifiers = List.map (fun (_, f) -> "JsInterpreterMonads." ^ f) monad_mapping
