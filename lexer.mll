{
  open Parser

  exception DiaLexError of string

  let dia_dbgprint str = Printf.eprintf "[lexer.mll] %s\n" str
}

let digit = ['0'-'9']

rule parse_code = parse
| ' ' { parse_code lexbuf }
| '\n' { parse_code lexbuf }
| '#' { parse_code lexbuf }
| "/*" { parse_code lexbuf }
| "0x" ['0'-'9' 'a'-'f' 'A'-'F'] as hex {
    dia_dbgprint ("DIA_HEXADECIMAL: " ^ hex);
    DIA_INTEGER {
      name = hex;
      token_type = DiaConstant DiaInteger;
      num_of_parameters = 0;
      parameters = [];
      next_function = None;
    }
  }
| '-'? ['0'-'9']+ as hex {
    dia_dbgprint ("DIA_INTEGER: " ^ hex);
    DIA_INTEGER {
      name = hex;
      token_type = DiaConstant DiaInteger;
      num_of_parameters = 0;
      parameters = [];
      next_function = None;
    }
  }
| digit+ '.' digit+ as double {
    dia_dbgprint ("DIA_DOUBLE: " ^ double);
    DIA_DOUBLE {
      name = double;
      token_type = DiaConstant DiaDouble;
      num_of_parameters = 0;
      parameters = [];
      next_function = None;
    }
  }
| '"' ( [^ '\\' '"'] | "\\.")* '"' as str {
    dia_dbgprint ("DIA_STRING: " ^ str);
    DIA_STRING {
      name = str;
      token_type = DiaConstant DiaString;
      num_of_parameters = 0;
      parameters = [];
      next_function = None;
    }
  }
| eof { EOF }
| "main"  { dia_dbgprint "DIA_MAIN_FUNC"; DIA_MAIN_FUNC }
| '='     { dia_dbgprint "DIA_ALLOC"; DIA_ALLOC }
| '+'     { dia_dbgprint "DIA_PLUS"; DIA_PLUS}
| '-'     { dia_dbgprint "DIA_MINUS"; DIA_MINUS }
| '*'     { dia_dbgprint "DIA_MUL"; DIA_MUL }
| '/'     { dia_dbgprint "DIA_DIV"; DIA_DIV }
| '%'     { dia_dbgprint "DIA_MOD"; DIA_MOD }
| "&&"    { dia_dbgprint "DIA_LOGICAL_AND"; DIA_LOGICAL_AND }
| "||"    { dia_dbgprint "DIA_LOGICAL_OR"; DIA_LOGICAL_OR }
| '!'     { dia_dbgprint "DIA_LOGICAL_NOT"; DIA_LOGICAL_NOT }
| "=="    { dia_dbgprint "DIA_EQUAL"; DIA_EQUAL }
| "!="    { dia_dbgprint "DIA_MINUS"; DIA_MINUS }
| ">="    { dia_dbgprint "DIA_GREATER_EQUAL"; DIA_GREATER_EQUAL }
| '>'     { dia_dbgprint "DIA_GREATER"; DIA_GREATER }
| "<="    { dia_dbgprint "DIA_LESS_EQUAL"; DIA_LESS_EQUAL }
| '<'     { dia_dbgprint "DIA_LESS"; DIA_LESS }
| '&'     { dia_dbgprint "DIA_BIT_AND"; DIA_BIT_AND }
| '^'     { dia_dbgprint "DIA_BIT_XOR"; DIA_BIT_XOR }
| '|'     { dia_dbgprint "DIA_BIT_OR"; DIA_BIT_OR }
| '~'     { dia_dbgprint "DIA_BIT_NOT"; DIA_BIT_NOT }
| ','     { dia_dbgprint "DIA_COMMA"; DIA_COMMA }
| '.'     { dia_dbgprint "DIA_BIND"; DIA_BIND }
| ';'     { dia_dbgprint "DIA_NEXT"; DIA_NEXT }
| '('     { dia_dbgprint "DIA_OPEN_PARENTHESIS"; DIA_OPEN_PARENTHESIS }
| ')'     { dia_dbgprint "DIA_CLOSE_PARENTHESIS"; DIA_CLOSE_PARENTHESIS }
| '['     { dia_dbgprint "DIA_OPEN_BRACKET"; DIA_OPEN_BRACKET }
| ']'     { dia_dbgprint "DIA_CLOSE_BRACKET"; DIA_CLOSE_BRACKET }
| ':'     { dia_dbgprint "DIA_COLON"; DIA_COLON }
| "if"    { dia_dbgprint "DIA_IF"; DIA_IF }
| "else"  { dia_dbgprint "DIA_ELSE"; DIA_ELSE }

| ('T'|'t') "rue" as tr {
    dia_dbgprint ("DIA_BOOL: " ^ tr);
    DIA_BOOL {
      name = tr;
      token_type = DiaConstant DiaBool;
      num_of_parameters = 0;
      parameters = [];
      next_function = None;
    }
  }
| ('F'|'f') "alse" as fl {
    dia_dbgprint ("DIA_BOOL: " ^ fl);
    DIA_BOOL {
      name = fl;
      token_type = DiaConstant DiaBool;
      num_of_parameters = 0;
      parameters = [];
      next_function = None;
    }
  }
| ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']* as id {
    dia_dbgprint ("DIA_IDENTIFIER: " ^ id);
    DIA_IDENTIFIER id
  }
| _ {
    let err_str = Printf.sprintf
      "Unexpected token %d-%d: %s" (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) (Lexing.lexeme lexbuf)
    in raise (DiaLexError err_str)
  }
