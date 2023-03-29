(* Datatypes for Postscript instructions *)


open Lang

type instr =
  IVal of value
| IVar of int
| IOper of string
| IBloc of instr
| ISeq of instr list
| ILoop of instr
| IDef of fname * instr

let rec string_of_instr instr = match instr with 
	Ival val -> (match val with
				BootT b -> Bool.to_string(b)
				|FloatV f -> string_of_float(f)
				|IntV i-> string_of_int (i)
				|LitV l -> l
				StringV s -> s)
	|IVar i -> string_of_int(i)
	|IOper op -> op
	|IBloc instr -> string_of_instr instr
	|ISeq l -> 
