open Dia
open DiaNode

exception DiaFileNotFoundError of string

let dia_help = "\
  Dia: The brilliance of Diamond, the elegance of the Language\n\n\
  Usage: diac [options] file.dia\n\n\
  Option:\n\
    -v[v..]       Increase the level of verbosity (-v to -vvvv)\n\
    --stdout      Print compiled code to terminal\n\
    --no-compile  Do not compile, persist the source code.\n\
    --skip-format Skip formatting the generated code using clang-format.\n\
    --quiet       Do not generate file comment.\n\
    --static      Generate static binary (equivalent of --static option in C++ compilers)\n\n\
  Experimental Options:\n\
    --pure        Add [[gnu::pure]] macro to all custom functions.\n\
    --constexpr   Add constexpr keyword to all custom functions.\n\
    --faster-io   Disable input and output buffer to increase I/O performance.\n\
                  (Should be used on competitive programming)\n\
"

let generate_header = "#include <iostream>\n\
  #include <string>\n\
  #include <vector>\n\
  #include <algorithm>\n\
"

type dia_cli_options = {
  input_file: string ref;
  output_file: string ref;
  verbosity: bool ref;
  std_out: bool ref;
  no_compile: bool ref;
  skip_format: bool ref;
  quiet: bool ref;
  static: bool ref;
  pure: bool ref;
  constexpr: bool ref;
  faster_io: bool ref;
}

let cli_options = {
  input_file = ref "";
  output_file = ref "";
  verbosity = ref false;
  std_out = ref false;
  no_compile = ref false;
  skip_format = ref false;
  quiet = ref false;
  static = ref false;
  pure = ref false;
  constexpr = ref false;
  faster_io = ref false;
}

let speclist = [
  ("-v", Arg.Set cli_options.verbosity, "Increase the level of verbosity (-v to -vvvv)");
  ("-o", Arg.Set_string cli_options.output_file, "Set explicit output file name.");
  ("--stdout", Arg.Set cli_options.std_out, "Print compiled code to terminal");
  ("--no-compile", Arg.Set cli_options.no_compile, "Do not compile, persist the source code.");
  ("--skip-format", Arg.Set cli_options.skip_format, "Skip formatting the generated code using clang-format.");
  ("--quiet", Arg.Set cli_options.quiet, "Do not generate file comment.");
  ("--static", Arg.Set cli_options.static, "Generate static binary (equivalent of --static option in C++ compilers)");
  (* Experimental Options *)
  ("--pure", Arg.Set cli_options.pure, "Add [[gnu::pure]] macro to all custom functions.");
  ("--constexpr", Arg.Set cli_options.constexpr, "Add constexpr keyword to all custom functions.");
  ("--faster-io", Arg.Set cli_options.faster_io, "Disable input and output buffer to increase I/O performance.\n(Should be used on competitive programming)");
]

let _ =
  let () = Arg.parse speclist (fun str -> cli_options.input_file := str) dia_help in
  let ch = if cli_options.input_file.contents = ""
    then raise (DiaFileNotFoundError "Dia: No input file given. Example: diac <file>.dia")
    else open_in cli_options.input_file.contents
  in
  let dia_file = In_channel.input_all ch in
  let _ = In_channel.close ch in
  let lexbuf = Lexing.from_string dia_file in
  while true do 
    let result = Parser.dia Lexer.parse_code lexbuf in
      print_endline generate_header;
      List.iter (fun f -> dia_custom_function f result.custom_functions) result.custom_functions;
      dia_main result dia_file
  done