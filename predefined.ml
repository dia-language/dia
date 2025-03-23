open DiaNode

type dia_predefined_function = {
  node: dia_node;
  generate_code: dia_node -> int -> dia_node * int;
}

let dia_dbgprint str = Printf.eprintf "[predefined.ml] %s\n" str

let var_counter_init = 0

let dia_create_cpp_variable (i: int) =
  Printf.sprintf "v%d" i

(* The Predefined.functions is defined at the end of the file. *)

(*
 * Functions - IO Starts
 *)
let functions_io = [
  {
    node = {
      name = "print";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = -1;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      dia_dbgprint "Generating 'print' function";
      print_string "std::cout";
      List.iter (fun n -> print_string("<<" ^ n.name)) node.parameters;
      print_endline ";";
      {
        name = "_";
        token_type = DiaConstant DiaVoid;
        num_of_parameters = 0;
        parameters = [];
        next_function = None;
      }, var_index
  };
  {
    node = {
      name = "puts";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = -1;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      dia_dbgprint "Generating 'puts' function";
      print_string "std::cout";
      List.iter (fun n -> print_string("<<" ^ n.name)) node.parameters;
      print_endline "<<std::endl;";
      {
        name = "_";
        token_type = DiaConstant DiaVoid;
        num_of_parameters = 0;
        parameters = [];
        next_function = None;
      }, var_index
  };
]
(*
 * Functions - IO Ends
 *)

(*
 * Functions - Arithmetic Starts
 *)
let functions_arithmetic = [
  {
    node = {
      name = "plus";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'plus' function";
      Printf.printf "auto %s=%s+%s;\n"
        (dia_create_cpp_variable var_index)
        a1.name
        a2.name;
      {
        name = (dia_create_cpp_variable var_index);
        token_type = a1.token_type;
        num_of_parameters = 0;
        parameters = [];
        next_function = None;
      }, var_index+1
  };

]
(*
 * Functions - Arithmetic Ends
 *)


let functions =
  functions_io
  @ functions_arithmetic