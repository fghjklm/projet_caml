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
	IVal valeur -> (match valeur with
				BoolV b -> Bool.to_string(b)
				|FloatV f -> string_of_float(f)
				|IntV i-> string_of_int (i)
				|LitV l -> l
				|StringV s -> s)
	|IVar i -> string_of_int(i)^" index"
	|IOper op -> op
	|IBloc instr -> string_of_instr instr
	|ISeq l -> 
	let rec aux_string_of_inst_ISEQ l = (match l with
			(a::q) -> (string_of_instr a)^" "^(aux_string_of_inst_ISEQ q)
			|[] -> "") in aux_string_of_inst_ISEQ l
	|ILoop l -> "{"^(string_of_instr l)^"} loop"
	|IDef (name,inst) -> "/"^name^"	{"^(string_of_instr inst)^"} def"

	
	


