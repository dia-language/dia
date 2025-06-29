type dia_basic_type = DiaInteger | DiaDouble | DiaString | DiaBool | DiaVoid

type dia_token_type =
  | DiaFunction of dia_basic_type
  | DiaFunctionParam of dia_basic_type
  | DiaConstant of dia_basic_type

type dia_node = {
  name : string;
  token_type : dia_token_type;
  num_of_parameters : int;
  parameters : dia_node list;
  next_function : dia_node option;
}

type dia = { custom_functions : dia_node list; main_function : dia_node }
