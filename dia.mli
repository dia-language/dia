val dia_dbgprint : string -> unit

val dia_debug_function_descriptor : DiaNode.dia_node -> int -> unit

val dia_generate_code : DiaNode.dia_node -> DiaNode.dia_node list -> int -> DiaNode.dia_node * int

val dia_main : DiaNode.dia -> string -> unit

val dia_custom_function : DiaNode.dia_node -> DiaNode.dia_node list -> unit
