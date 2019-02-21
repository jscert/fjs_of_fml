(* 

- Test : 'ocamlbuild -I src -use-ocamlfind visitor.native -cflag -dsource'

- List of unsuppored constructors (from js_of_ast.ml)
   Texp_assert    |!Texp_match!|!Texp_match! | Texp_try     | Texp_function   | Texp_variant  |
   Texp_setfield  | Texp_send  | Texp_new    | Texp_instvar | Texp_setinstvar | Texp_override |
   Texp_letmodule | Texp_lazy  | Texp_object | Texp_pack

 ==> As a consequence making Texp_instvar, Texp_setinstvar, Texp_override 'string Asttypes.loc' 
     field opaque is fine

*) 

open Typedtree

type expression_desc = Typedtree.expression_desc =
  | Texp_ident of (Path.t [@name "pt"]) * (Longident.t [@name "lt"]) Asttypes.loc * Types.value_description 
  | Texp_constant of Asttypes.constant 
  | Texp_let of Asttypes.rec_flag * value_binding list * expression 
  | Texp_function of
  {
  arg_label: Asttypes.arg_label ;
  param: (Ident.t [@name "it"]) ;
  cases: case list ;
  partial: partial }
  | Texp_apply of expression * (Asttypes.arg_label * expression option) list
  | Texp_match of expression * case list * case list * partial 
  | Texp_try of expression * case list 
  | Texp_tuple of expression list 
  | Texp_construct of (Longident.t [@name "lt"]) Asttypes.loc  *
  Types.constructor_description * expression list 
  | Texp_variant of Asttypes.label * expression option 
  | Texp_record of
  {
  fields: (Types.label_description * record_label_definition) array ;
  representation: Types.record_representation ;
  extended_expression: expression option } 
  | Texp_field of expression * (Longident.t [@name "lt"]) Asttypes.loc  *
  Types.label_description 
  | Texp_setfield of expression * (Longident.t [@name "lt"]) Asttypes.loc  *
  Types.label_description * expression 
  | Texp_array of expression list 
  | Texp_ifthenelse of expression * expression * expression option 
  | Texp_sequence of expression * expression 
  | Texp_while of expression * expression 
  | Texp_for of (Ident.t [@name "it"]) * Parsetree.pattern * expression * expression *
  Asttypes.direction_flag * expression 
  | Texp_send of expression * meth * expression option 
  | Texp_new of(Path.t [@name "pt"]) * (Longident.t [@name "lt"]) Asttypes.loc  * Types.class_declaration 
  | Texp_instvar of (Path.t [@name "pt"]) * (Path.t [@name "pt"]) * (string Asttypes.loc [@opaque]) 
  | Texp_setinstvar of (Path.t [@name "pt"]) * (Path.t [@name "pt"]) * (string Asttypes.loc [@opaque]) * expression 
  | Texp_override of (Path.t [@name "pt"]) * ((Path.t [@name "pt"]) * (string Asttypes.loc [@opaque]) * expression)
  list 
  | Texp_letmodule of (Ident.t [@name "it"]) * (string Asttypes.loc [@opaque]) * module_expr *
  expression 
  | Texp_letexception of extension_constructor * expression 
  | Texp_assert of expression 
  | Texp_lazy of expression 
  | Texp_object of class_structure * string list 
  | Texp_pack of module_expr 
  | Texp_unreachable 
  | Texp_extension_constructor of (Longident.t [@name "lt"]) Asttypes.loc * (Path.t [@name "pt"])
  [@@deriving visitors { name = "map_expression_desc" ; variety = "map" }]

(* Generated code : 

include
  struct
    [@@@ocaml.warning "-4-26-27"]
    [@@@VISITORS.BEGIN ]
    class virtual ['self] map_expression_desc =
      object (self : 'self)
        inherit  [_] VisitorsRuntime.map
        method visit_Texp_ident env _visitors_c0 _visitors_c1 _visitors_c2 =
          let _visitors_r0 = self#visit_pt env _visitors_c0 in
          let _visitors_r1 = self#visit_loc self#visit_lt env _visitors_c1 in
          let _visitors_r2 = self#visit_value_description env _visitors_c2 in
          Texp_ident (_visitors_r0, _visitors_r1, _visitors_r2)
        method visit_Texp_constant env _visitors_c0 =
          let _visitors_r0 = self#visit_constant env _visitors_c0 in
          Texp_constant _visitors_r0
        method visit_Texp_let env _visitors_c0 _visitors_c1 _visitors_c2 =
          let _visitors_r0 = self#visit_rec_flag env _visitors_c0 in
          let _visitors_r1 =
            self#visit_list self#visit_value_binding env _visitors_c1 in
          let _visitors_r2 = self#visit_expression env _visitors_c2 in
          Texp_let (_visitors_r0, _visitors_r1, _visitors_r2)
        method visit_Texp_function env _visitors_farg_label _visitors_fparam
          _visitors_fcases _visitors_fpartial =
          let _visitors_r0 = self#visit_arg_label env _visitors_farg_label in
          let _visitors_r1 = self#visit_it env _visitors_fparam in
          let _visitors_r2 =
            self#visit_list self#visit_case env _visitors_fcases in
          let _visitors_r3 = self#visit_partial env _visitors_fpartial in
          Texp_function
            {
              arg_label = _visitors_r0;
              param = _visitors_r1;
              cases = _visitors_r2;
              partial = _visitors_r3
            }
        method visit_Texp_apply env _visitors_c0 _visitors_c1 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 =
            self#visit_list
              (fun env ->
                 fun (_visitors_c0, _visitors_c1) ->
                   let _visitors_r0 = self#visit_arg_label env _visitors_c0 in
                   let _visitors_r1 =
                     self#visit_option self#visit_expression env _visitors_c1 in
                   (_visitors_r0, _visitors_r1)) env _visitors_c1 in
          Texp_apply (_visitors_r0, _visitors_r1)
        method visit_Texp_match env _visitors_c0 _visitors_c1 _visitors_c2
          _visitors_c3 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 = self#visit_list self#visit_case env _visitors_c1 in
          let _visitors_r2 = self#visit_list self#visit_case env _visitors_c2 in
          let _visitors_r3 = self#visit_partial env _visitors_c3 in
          Texp_match (_visitors_r0, _visitors_r1, _visitors_r2, _visitors_r3)
        method visit_Texp_try env _visitors_c0 _visitors_c1 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 = self#visit_list self#visit_case env _visitors_c1 in
          Texp_try (_visitors_r0, _visitors_r1)
        method visit_Texp_tuple env _visitors_c0 =
          let _visitors_r0 =
            self#visit_list self#visit_expression env _visitors_c0 in
          Texp_tuple _visitors_r0
        method visit_Texp_construct env _visitors_c0 _visitors_c1
          _visitors_c2 =
          let _visitors_r0 = self#visit_loc self#visit_lt env _visitors_c0 in
          let _visitors_r1 =
            self#visit_constructor_description env _visitors_c1 in
          let _visitors_r2 =
            self#visit_list self#visit_expression env _visitors_c2 in
          Texp_construct (_visitors_r0, _visitors_r1, _visitors_r2)
        method visit_Texp_variant env _visitors_c0 _visitors_c1 =
          let _visitors_r0 = self#visit_label env _visitors_c0 in
          let _visitors_r1 =
            self#visit_option self#visit_expression env _visitors_c1 in
          Texp_variant (_visitors_r0, _visitors_r1)
        method visit_Texp_record env _visitors_ffields
          _visitors_frepresentation _visitors_fextended_expression =
          let _visitors_r0 =
            self#visit_array
              (fun env ->
                 fun (_visitors_c0, _visitors_c1) ->
                   let _visitors_r0 =
                     self#visit_label_description env _visitors_c0 in
                   let _visitors_r1 =
                     self#visit_record_label_definition env _visitors_c1 in
                   (_visitors_r0, _visitors_r1)) env _visitors_ffields in
          let _visitors_r1 =
            self#visit_record_representation env _visitors_frepresentation in
          let _visitors_r2 =
            self#visit_option self#visit_expression env
              _visitors_fextended_expression in
          Texp_record
            {
              fields = _visitors_r0;
              representation = _visitors_r1;
              extended_expression = _visitors_r2
            }
        method visit_Texp_field env _visitors_c0 _visitors_c1 _visitors_c2 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 = self#visit_loc self#visit_lt env _visitors_c1 in
          let _visitors_r2 = self#visit_label_description env _visitors_c2 in
          Texp_field (_visitors_r0, _visitors_r1, _visitors_r2)
        method visit_Texp_setfield env _visitors_c0 _visitors_c1 _visitors_c2
          _visitors_c3 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 = self#visit_loc self#visit_lt env _visitors_c1 in
          let _visitors_r2 = self#visit_label_description env _visitors_c2 in
          let _visitors_r3 = self#visit_expression env _visitors_c3 in
          Texp_setfield
            (_visitors_r0, _visitors_r1, _visitors_r2, _visitors_r3)
        method visit_Texp_array env _visitors_c0 =
          let _visitors_r0 =
            self#visit_list self#visit_expression env _visitors_c0 in
          Texp_array _visitors_r0
        method visit_Texp_ifthenelse env _visitors_c0 _visitors_c1
          _visitors_c2 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 = self#visit_expression env _visitors_c1 in
          let _visitors_r2 =
            self#visit_option self#visit_expression env _visitors_c2 in
          Texp_ifthenelse (_visitors_r0, _visitors_r1, _visitors_r2)
        method visit_Texp_sequence env _visitors_c0 _visitors_c1 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 = self#visit_expression env _visitors_c1 in
          Texp_sequence (_visitors_r0, _visitors_r1)
        method visit_Texp_while env _visitors_c0 _visitors_c1 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 = self#visit_expression env _visitors_c1 in
          Texp_while (_visitors_r0, _visitors_r1)
        method visit_Texp_for env _visitors_c0 _visitors_c1 _visitors_c2
          _visitors_c3 _visitors_c4 _visitors_c5 =
          let _visitors_r0 = self#visit_it env _visitors_c0 in
          let _visitors_r1 = self#visit_pattern env _visitors_c1 in
          let _visitors_r2 = self#visit_expression env _visitors_c2 in
          let _visitors_r3 = self#visit_expression env _visitors_c3 in
          let _visitors_r4 = self#visit_direction_flag env _visitors_c4 in
          let _visitors_r5 = self#visit_expression env _visitors_c5 in
          Texp_for
            (_visitors_r0, _visitors_r1, _visitors_r2, _visitors_r3,
              _visitors_r4, _visitors_r5)
        method visit_Texp_send env _visitors_c0 _visitors_c1 _visitors_c2 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          let _visitors_r1 = self#visit_meth env _visitors_c1 in
          let _visitors_r2 =
            self#visit_option self#visit_expression env _visitors_c2 in
          Texp_send (_visitors_r0, _visitors_r1, _visitors_r2)
        method visit_Texp_new env _visitors_c0 _visitors_c1 _visitors_c2 =
          let _visitors_r0 = self#visit_pt env _visitors_c0 in
          let _visitors_r1 = self#visit_loc self#visit_lt env _visitors_c1 in
          let _visitors_r2 = self#visit_class_declaration env _visitors_c2 in
          Texp_new (_visitors_r0, _visitors_r1, _visitors_r2)
        method visit_Texp_instvar env _visitors_c0 _visitors_c1 _visitors_c2
          =
          let _visitors_r0 = self#visit_pt env _visitors_c0 in
          let _visitors_r1 = self#visit_pt env _visitors_c1 in
          let _visitors_r2 =
            (fun _visitors_this -> _visitors_this) _visitors_c2 in
          Texp_instvar (_visitors_r0, _visitors_r1, _visitors_r2)
        method visit_Texp_setinstvar env _visitors_c0 _visitors_c1
          _visitors_c2 _visitors_c3 =
          let _visitors_r0 = self#visit_pt env _visitors_c0 in
          let _visitors_r1 = self#visit_pt env _visitors_c1 in
          let _visitors_r2 =
            (fun _visitors_this -> _visitors_this) _visitors_c2 in
          let _visitors_r3 = self#visit_expression env _visitors_c3 in
          Texp_setinstvar
            (_visitors_r0, _visitors_r1, _visitors_r2, _visitors_r3)
        method visit_Texp_override env _visitors_c0 _visitors_c1 =
          let _visitors_r0 = self#visit_pt env _visitors_c0 in
          let _visitors_r1 =
            self#visit_list
              (fun env ->
                 fun (_visitors_c0, _visitors_c1, _visitors_c2) ->
                   let _visitors_r0 = self#visit_pt env _visitors_c0 in
                   let _visitors_r1 =
                     (fun _visitors_this -> _visitors_this) _visitors_c1 in
                   let _visitors_r2 = self#visit_expression env _visitors_c2 in
                   (_visitors_r0, _visitors_r1, _visitors_r2)) env
              _visitors_c1 in
          Texp_override (_visitors_r0, _visitors_r1)
        method visit_Texp_letmodule env _visitors_c0 _visitors_c1
          _visitors_c2 _visitors_c3 =
          let _visitors_r0 = self#visit_it env _visitors_c0 in
          let _visitors_r1 =
            (fun _visitors_this -> _visitors_this) _visitors_c1 in
          let _visitors_r2 = self#visit_module_expr env _visitors_c2 in
          let _visitors_r3 = self#visit_expression env _visitors_c3 in
          Texp_letmodule
            (_visitors_r0, _visitors_r1, _visitors_r2, _visitors_r3)
        method visit_Texp_letexception env _visitors_c0 _visitors_c1 =
          let _visitors_r0 =
            self#visit_extension_constructor env _visitors_c0 in
          let _visitors_r1 = self#visit_expression env _visitors_c1 in
          Texp_letexception (_visitors_r0, _visitors_r1)
        method visit_Texp_assert env _visitors_c0 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          Texp_assert _visitors_r0
        method visit_Texp_lazy env _visitors_c0 =
          let _visitors_r0 = self#visit_expression env _visitors_c0 in
          Texp_lazy _visitors_r0
        method visit_Texp_object env _visitors_c0 _visitors_c1 =
          let _visitors_r0 = self#visit_class_structure env _visitors_c0 in
          let _visitors_r1 =
            self#visit_list self#visit_string env _visitors_c1 in
          Texp_object (_visitors_r0, _visitors_r1)
        method visit_Texp_pack env _visitors_c0 =
          let _visitors_r0 = self#visit_module_expr env _visitors_c0 in
          Texp_pack _visitors_r0
        method visit_Texp_unreachable env = Texp_unreachable
        method visit_Texp_extension_constructor env _visitors_c0 _visitors_c1
          =
          let _visitors_r0 = self#visit_loc self#visit_lt env _visitors_c0 in
          let _visitors_r1 = self#visit_pt env _visitors_c1 in
          Texp_extension_constructor (_visitors_r0, _visitors_r1)
        method visit_expression_desc env _visitors_this =
          match _visitors_this with
          | Texp_ident (_visitors_c0, _visitors_c1, _visitors_c2) ->
              self#visit_Texp_ident env _visitors_c0 _visitors_c1
                _visitors_c2
          | Texp_constant _visitors_c0 ->
              self#visit_Texp_constant env _visitors_c0
          | Texp_let (_visitors_c0, _visitors_c1, _visitors_c2) ->
              self#visit_Texp_let env _visitors_c0 _visitors_c1 _visitors_c2
          | Texp_function
              { arg_label = _visitors_farg_label; param = _visitors_fparam;
                cases = _visitors_fcases; partial = _visitors_fpartial }
              ->
              self#visit_Texp_function env _visitors_farg_label
                _visitors_fparam _visitors_fcases _visitors_fpartial
          | Texp_apply (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_apply env _visitors_c0 _visitors_c1
          | Texp_match
              (_visitors_c0, _visitors_c1, _visitors_c2, _visitors_c3) ->
              self#visit_Texp_match env _visitors_c0 _visitors_c1
                _visitors_c2 _visitors_c3
          | Texp_try (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_try env _visitors_c0 _visitors_c1
          | Texp_tuple _visitors_c0 -> self#visit_Texp_tuple env _visitors_c0
          | Texp_construct (_visitors_c0, _visitors_c1, _visitors_c2) ->
              self#visit_Texp_construct env _visitors_c0 _visitors_c1
                _visitors_c2
          | Texp_variant (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_variant env _visitors_c0 _visitors_c1
          | Texp_record
              { fields = _visitors_ffields;
                representation = _visitors_frepresentation;
                extended_expression = _visitors_fextended_expression }
              ->
              self#visit_Texp_record env _visitors_ffields
                _visitors_frepresentation _visitors_fextended_expression
          | Texp_field (_visitors_c0, _visitors_c1, _visitors_c2) ->
              self#visit_Texp_field env _visitors_c0 _visitors_c1
                _visitors_c2
          | Texp_setfield
              (_visitors_c0, _visitors_c1, _visitors_c2, _visitors_c3) ->
              self#visit_Texp_setfield env _visitors_c0 _visitors_c1
                _visitors_c2 _visitors_c3
          | Texp_array _visitors_c0 -> self#visit_Texp_array env _visitors_c0
          | Texp_ifthenelse (_visitors_c0, _visitors_c1, _visitors_c2) ->
              self#visit_Texp_ifthenelse env _visitors_c0 _visitors_c1
                _visitors_c2
          | Texp_sequence (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_sequence env _visitors_c0 _visitors_c1
          | Texp_while (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_while env _visitors_c0 _visitors_c1
          | Texp_for
              (_visitors_c0, _visitors_c1, _visitors_c2, _visitors_c3,
               _visitors_c4, _visitors_c5)
              ->
              self#visit_Texp_for env _visitors_c0 _visitors_c1 _visitors_c2
                _visitors_c3 _visitors_c4 _visitors_c5
          | Texp_send (_visitors_c0, _visitors_c1, _visitors_c2) ->
              self#visit_Texp_send env _visitors_c0 _visitors_c1 _visitors_c2
          | Texp_new (_visitors_c0, _visitors_c1, _visitors_c2) ->
              self#visit_Texp_new env _visitors_c0 _visitors_c1 _visitors_c2
          | Texp_instvar (_visitors_c0, _visitors_c1, _visitors_c2) ->
              self#visit_Texp_instvar env _visitors_c0 _visitors_c1
                _visitors_c2
          | Texp_setinstvar
              (_visitors_c0, _visitors_c1, _visitors_c2, _visitors_c3) ->
              self#visit_Texp_setinstvar env _visitors_c0 _visitors_c1
                _visitors_c2 _visitors_c3
          | Texp_override (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_override env _visitors_c0 _visitors_c1
          | Texp_letmodule
              (_visitors_c0, _visitors_c1, _visitors_c2, _visitors_c3) ->
              self#visit_Texp_letmodule env _visitors_c0 _visitors_c1
                _visitors_c2 _visitors_c3
          | Texp_letexception (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_letexception env _visitors_c0 _visitors_c1
          | Texp_assert _visitors_c0 ->
              self#visit_Texp_assert env _visitors_c0
          | Texp_lazy _visitors_c0 -> self#visit_Texp_lazy env _visitors_c0
          | Texp_object (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_object env _visitors_c0 _visitors_c1
          | Texp_pack _visitors_c0 -> self#visit_Texp_pack env _visitors_c0
          | Texp_unreachable -> self#visit_Texp_unreachable env
          | Texp_extension_constructor (_visitors_c0, _visitors_c1) ->
              self#visit_Texp_extension_constructor env _visitors_c0
                _visitors_c1
      end
    [@@@VISITORS.END ]
  end
 *)
