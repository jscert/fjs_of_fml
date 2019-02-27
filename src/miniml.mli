type miniml_expression

type miniml_expression_desc

type miniml_value_binding

type miniml_pattern

type miniml_pattern_desc

type miniml_record_label_definition

val ml2mml_exp : Typedtree.expression -> miniml_expression

val ml2mml_exp_desc : Typedtree.expression_desc -> miniml_expression_desc

val ml2mml_value_binding : Typedtree.value_binding -> miniml_value_binding

val ml2mml_pattern : Typedtree.pattern -> miniml_pattern

val ml2mml_pattern_desc : Typedtree.pattern_desc -> miniml_pattern_desc

val ml2mml_record_label_definition :
  Typedtree.record_label_definition -> miniml_record_label_definition
