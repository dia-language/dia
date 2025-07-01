open Alcotest
open Dia

let test_create_cpp_variable () =
  check string "variable v0" "v0" (dia_create_cpp_variable 0);
  check string "variable v42" "v42" (dia_create_cpp_variable 42)

let () =
  run "Dia" [
    ("dia_create_cpp_variable",
     [test_case "creates variables" `Quick test_create_cpp_variable]);
  ]
