%{
  open DiaNode

  exception DiaSyntaxError of string

  let dia_dbgprint str = Printf.eprintf "[parser.mly] %s\n" str

  let string_to_type (str: string) =
  match str with
  | "int" -> DiaInteger
  | "string" -> DiaString
  | "double" -> DiaDouble
  | "bool" -> DiaBool
  | "void" -> DiaVoid
  | _ as tk -> raise (DiaSyntaxError (Printf.sprintf "Unknown type '%s'" tk))

  let rec weave_function_parameter (node: DiaNode.dia_node) (params: DiaNode.dia_node list) =
    let result = List.find_opt (fun p -> p.name = node.name) params in
    let weaved_params = List.map (fun p -> weave_function_parameter p params) node.parameters in
    let weaved_next_func = match node.next_function with
      | None -> None
      | Some fn -> Some (weave_function_parameter fn params)
    in
    dia_dbgprint ("Trying to weave " ^ node.name);
    match result with
    | None -> {
        name = node.name;
        token_type = node.token_type;
        num_of_parameters = node.num_of_parameters;
        parameters = weaved_params;
        next_function = weaved_next_func;
      }
    | Some _ ->
      dia_dbgprint "because I found something.";
      {
        name = node.name;
        token_type = (match node.token_type with
        | DiaFunction DiaInteger -> DiaFunctionParam DiaInteger
        | DiaFunction DiaString -> DiaFunctionParam DiaString
        | DiaFunction DiaDouble -> DiaFunctionParam DiaDouble
        | DiaFunction DiaBool -> DiaFunctionParam DiaBool
        | DiaFunction DiaVoid -> DiaFunctionParam DiaVoid
        | _ -> node.token_type
        );
        num_of_parameters = node.num_of_parameters;
        parameters = weaved_params;
        next_function = weaved_next_func;
      }
%}

%token <DiaNode.dia_node> DIA_INTEGER
%token <DiaNode.dia_node> DIA_DOUBLE
%token <DiaNode.dia_node> DIA_STRING
%token <DiaNode.dia_node> DIA_BOOL
%token <DiaNode.dia_node> DIA_VOID
%token EOF

%token DIA_MAIN_FUNC         /* "main" */
%token DIA_ALLOC             /* "=" */

/* Calculation */
%token DIA_PLUS              /* "+" */
%token DIA_MINUS             /* "-" */
%token DIA_MUL               /* "*" */
%token DIA_DIV               /* "/" */
%token DIA_MOD               /* "%" */

/* Logical */
%token DIA_LOGICAL_AND       /* "&&" */
%token DIA_LOGICAL_OR        /* "||" */
%token DIA_LOGICAL_NOT       /* "!" */

/* Conditional */
%token DIA_EQUAL             /* "==" */
%token DIA_NOT_EQUAL         /* "!=" */
%token DIA_GREATER_EQUAL     /* ">=" */
%token DIA_GREATER           /* ">" */
%token DIA_LESS_EQUAL        /* "<=" */
%token DIA_LESS              /* "<" */

/* Bit */
%token DIA_BIT_AND           /* "&" */
%token DIA_BIT_XOR           /* "^" */
%token DIA_BIT_OR            /* "|" */
%token DIA_BIT_NOT           /* "~" */

/* etc. */
%token DIA_COMMA             /* "," */
%token DIA_BIND              /* "." */
%token DIA_NEXT              /* ";" */

/* Brackets */
%token DIA_OPEN_PARENTHESIS  /* "(" */
%token DIA_CLOSE_PARENTHESIS /* ")" */
%token DIA_OPEN_BRACKET      /* "[" */
%token DIA_CLOSE_BRACKET     /* "]" */

%token DIA_COLON             /* ":" */

%token DIA_IF                /* "if" */
%token DIA_ELSE              /* "else" */

%token <string> DIA_IDENTIFIER   /* UNCLASSIFIED tokens */

/* Precedence */
/* Reference: https://en.cppreference.com/w/c/language/operator_precedence */
%left DIA_LOGICAL_OR
%left DIA_LOGICAL_AND

%left DIA_BIT_OR
%left DIA_BIT_XOR
%left DIA_BIT_AND

%left DIA_EQUAL DIA_NOT_EQUAL
%left DIA_GREATER_EQUAL DIA_GREATER DIA_LESS_EQUAL DIA_LESS

%left DIA_PLUS DIA_MINUS
%left DIA_MUL DIA_DIV DIA_MOD
%left DIA_BIND DIA_NEXT

%start dia
%type <DiaNode.dia> dia
%type <DiaNode.dia_node list> dia_parameters

%%

(*
 * dia -+-- custom_functions
 *      |
 *      +-- dia_main
 *)
dia:
| custom_functions dia_main {
    dia_dbgprint "Welcome to the Dia World!";
    List.iter (fun n -> Dia.dia_debug_function_descriptor n 0) $1;
    {
      custom_functions = $1;
      main_function = $2;
    }
  }
;

(* Custom Functions *)
custom_functions:
| (* If Empty *) { [] }
| DIA_IDENTIFIER DIA_OPEN_PARENTHESIS custom_parameters DIA_CLOSE_PARENTHESIS DIA_COLON DIA_IDENTIFIER DIA_ALLOC dia_expr custom_functions
  {
    dia_dbgprint "DIA_IDENTIFIER ( dia_parameters [:types], ... ) : type DIA_ALLOC dia_expr custom_functions";
    (weave_function_parameter {
      name = $1;      (* Use the captured identifier *)
      token_type = DiaFunction (string_to_type $6);
      num_of_parameters = List.length $3;
      parameters = $3;
      next_function = Some $8;
    } $3)
    :: $9
  }
| DIA_IDENTIFIER DIA_ALLOC dia_expr custom_functions
  {
    dia_dbgprint (Printf.sprintf "Custom function: which name is '%s'" $1);
    match $3.token_type with
    | DiaConstant ty -> {
        name = $1;
        token_type = DiaFunction ty;
        num_of_parameters = 0;
        parameters = [];
        next_function = Some $3;
      }
      :: $4
    | _ -> raise (DiaSyntaxError "Custom function without explicit function type notation should be a constant function.")
  }
;

custom_parameters:
| DIA_IDENTIFIER DIA_COLON DIA_IDENTIFIER
  {
    {
      name = $1;
      token_type = DiaFunctionParam (string_to_type $3);
      num_of_parameters = 0;
      parameters = [];
      next_function = None;
    }
    :: []
  }
| DIA_IDENTIFIER DIA_COLON DIA_IDENTIFIER DIA_COMMA custom_parameters
  {
    {
      name = $1;
      token_type = DiaFunctionParam (string_to_type $3);
      num_of_parameters = 0;
      parameters = [];
      next_function = None;
    }
    :: $5
  }

(* Dia Main *)
dia_main:
| DIA_MAIN_FUNC DIA_ALLOC dia_expr {
    dia_dbgprint "Main function detected";
    $3
  }
;

dia_expr:
| dia_expr DIA_BIND dia_expr
  {
    {
      name = $3.name;
      token_type = $3.token_type;
      num_of_parameters = $3.num_of_parameters + 1;
      parameters = $1 :: $3.parameters;
      next_function = $3.next_function;
    }
  }
| dia_expr DIA_NEXT dia_expr
  {
    {
      name = $1.name;
      token_type = $1.token_type;
      num_of_parameters = $1.num_of_parameters;
      parameters = $1.parameters;
      next_function = Some $3;
    }
  }
| dia_expr DIA_NEXT
  {
    dia_dbgprint "Semicolon Terminated";
    {
      name = $1.name;
      token_type = $1.token_type;
      num_of_parameters = $1.num_of_parameters;
      parameters = $1.parameters;
      next_function = None;
    }
  }
| dia_function { $1 }
;

dia_function:
| DIA_IDENTIFIER DIA_OPEN_PARENTHESIS dia_parameters DIA_CLOSE_PARENTHESIS
  {
    dia_dbgprint "Entering dia_function\n";
    {
      name = $1;
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 0;
      parameters = $3;
      next_function = None;
    }
  }
| DIA_IDENTIFIER {
    dia_dbgprint (
      Printf.sprintf "Looking for %s in predefined functions" $1
    );

    let pre_func = List.find_opt 
      (fun (e: Predefined.dia_predefined_function) -> e.node.name = $1)
      Predefined.functions
    in
    match pre_func with
    | None ->
      dia_dbgprint "...I will treat it as a custom function.";
      {
        name = $1;
        token_type = DiaFunction DiaVoid;
        num_of_parameters = 0;
        parameters = [];
        next_function = None;
      }
    | Some func -> dia_dbgprint "Found!"; func.node
  }
| dia_calculation { $1 }
| token { $1 }
;

dia_parameters:
| dia_function { $1 :: [] }
| dia_parameters DIA_COMMA dia_function { List.append $1 [$3] }
;

dia_calculation:
| dia_arithmetic { $1 }
| dia_logical    { $1 }
| dia_comparison { $1 }
| dia_bitwise    { $1 }
;

dia_arithmetic:
| dia_function DIA_PLUS dia_function {
    {
      name = "plus";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_MINUS dia_function {
    {
      name = "minus";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_MUL dia_function {
    {
      name = "multiplies";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_DIV dia_function {
    {
      name = "divides";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_MOD dia_function {
    {
      name = "modulus";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| DIA_MINUS dia_function {
    {
      name = "negate";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 1;
      parameters = [ $2; ];
      next_function = None;
    }
  }
;

dia_logical:
| dia_function DIA_LOGICAL_AND dia_function {
    {
      name = "logical_and";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_LOGICAL_OR dia_function {
    {
      name = "logical_or";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| DIA_LOGICAL_NOT dia_function {
    {
      name = "logical_not";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 1;
      parameters = [ $2 ];
      next_function = None;
    }
  }
;

dia_comparison:
| dia_function DIA_EQUAL dia_function {
    {
      name = "equal_to";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_NOT_EQUAL dia_function {
    {
      name = "not_equal_to";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_GREATER_EQUAL dia_function {
    {
      name = "greater_equal";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_GREATER dia_function {
    {
      name = "greater";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_LESS_EQUAL dia_function {
    {
      name = "less_equal";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_LESS dia_function {
    {
      name = "less";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
;

dia_bitwise:
| dia_function DIA_BIT_AND dia_function {
    {
      name = "bit_and";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_BIT_OR dia_function {
    {
      name = "bit_or";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| dia_function DIA_BIT_XOR dia_function {
    {
      name = "bit_xor";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 2;
      parameters = [ $1; $3 ];
      next_function = None;
    }
  }
| DIA_BIT_NOT dia_function {
    {
      name = "bit_not";
      token_type = DiaFunction DiaVoid;
      num_of_parameters = 1;
      parameters = [ $2 ];
      next_function = None;
    }
  }
;

token:
| DIA_STRING { $1 }
| DIA_INTEGER { $1 }
| DIA_DOUBLE { $1 }
| DIA_BOOL { $1 }
| DIA_OPEN_PARENTHESIS dia_calculation DIA_CLOSE_PARENTHESIS { $2 }
;

%%