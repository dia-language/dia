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
  Nearly all functions are defined like this:

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

  `node` was defined only for internal use; No token_type, no parameters,
  no next_function are needed. To generate code of the function, what one needed
  was just parameters.

  Return type of predefined functions is also dia_function. It is used for mana-
  ging `cpp_variable`s. The v1 in `auto v1 = 1+2;` for example.
 *)

(*
 * Functions - IO Start
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
 * Functions - IO End
 *)

(*
 * Functions - Arithmetic Start
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
  {
    node = {
      name = "minus";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'minus' function";
      Printf.printf "auto %s=%s-%s;\n"
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
  {
    node = {
      name = "multiplies";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'multiplies' function";
      Printf.printf "auto %s=%s* %s;\n"
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
  {
    node = {
      name = "divides";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'divides' function";
      Printf.printf "auto %s=%s/%s;\n"
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
  {
    node = {
      name = "modulus";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'modulus' function";
      Printf.printf "auto %s=%s%%%s;\n"
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
  {
    node = {
      name = "negate";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 1;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0
      in
      dia_dbgprint "Generating 'negate' function";
      Printf.printf "auto %s=%s*(-1);\n"
        (dia_create_cpp_variable var_index)
        a1.name;
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
 * Functions - Arithmetic End
 *)

(*
 * Functions - Logic Start
 *)

let functions_logic = [
  {
    node = {
      name = "logical_and";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'logical_and' function";
      Printf.printf "auto %s=%s&&%s;\n"
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
  {
    node = {
      name = "logical_or";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'logical_or' function";
      Printf.printf "auto %s=%s||%s;\n"
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
  {
    node = {
      name = "logical_not";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 1;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0
      in
      dia_dbgprint "Generating 'logical_not' function";
      Printf.printf "auto %s=!%s;\n"
        (dia_create_cpp_variable var_index)
        a1.name;
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
 * Functions - Logic End
 *)

(*
 * Functions - Comparison Start
 *)

 let functions_comparison = [
  {
    node = {
      name = "equal_to";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'equal_to' function";
      Printf.printf "auto %s=%s==%s;\n"
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
  {
    node = {
      name = "not_equal_to";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'not_equal_to' function";
      Printf.printf "auto %s=%s!=%s;\n"
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
  {
    node = {
      name = "greater_equal";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'greater_equal' function";
      Printf.printf "auto %s=%s>=%s;\n"
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
  {
    node = {
      name = "greater";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'greater' function";
      Printf.printf "auto %s=%s>%s;\n"
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
  {
    node = {
      name = "less_equal";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'less_equal' function";
      Printf.printf "auto %s=%s<=%s;\n"
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
  {
    node = {
      name = "less";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [];
      next_function = None;
    };
    generate_code = fun node var_index ->
      let a1 = List.nth node.parameters 0 in
      let a2 = List.nth node.parameters 1
      in
      dia_dbgprint "Generating 'less' function";
      Printf.printf "auto %s=%s<%s;\n"
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
 * Functions - Comparison End
 *)

let functions =
  functions_io
  @ functions_arithmetic
  @ functions_logic
  @ functions_comparison
