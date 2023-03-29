(* Typechecking of source programs *)

open Lang

(* Environments *)

type environment = 
    {localvars: (vname * tp) list; 
     funbind: fundecl list   
    }

let rec tp_of_expr contexte (exp:expr)= match exp with 
	Const (t:value) -> (match t with 
        BoolV _-> BoolT
        | FloatV _-> FloatT
        | IntV _-> IntT
        | LitV _-> LitT
        | StringV _-> StringT)
	|VarE (x:vname) -> (try List.assoc x contexte.localvars with Not_found -> failwith "variable non trouvée")
	|BinOp (binop,exp1,exp2) -> let tp1 = tp_of_expr contexte exp1 and tp2 = tp_of_expr contexte exp2 in 
                if tp1 = tp2 then (match binop with
                    BArith b -> if b = BAadd || b = BAsub || b = BAmul || b= BAdiv ||b= BAmod then 
                            if tp1 = tp2 && tp1 = IntT then IntT else failwith "erreur opération entière"
                        else
                            if tp1 = tp2 && tp1 = FloatT then FloatT else failwith "erreur opération flotante"
                    |BBool b -> if tp1 = tp2 && tp1 = BoolT then BoolT else failwith "erreur opération booléenne"    
                    |BCompar b -> if tp1 = tp2 then BoolT else failwith "erreur opération comparaison")
                else failwith "erreur opération type différents"
    |CondE (expcond,exp1,exp2) -> let tpcond = tp_of_expr contexte expcond and tp1 = tp_of_expr contexte exp1 and tp2 = tp_of_expr contexte exp2 in
                                if tpcond = BoolT then
                                    if tp1 = tp2 then tp1 else failwith "les deux branches doivent avoir le même type"                           
                                else failwith "le type d'une condition doit être un booléen"
    |CallE (fname, explist) -> (try 
                            let Fundecl(tp, name, args) = List.find ((function fname -> function Fundecl(tp,name,args) -> name = fname) fname) contexte.funbind 
                                
                            in
                            if List.length args = List.length explist && List.for_all2 (function Vardecl(a,tpa) -> function b -> a = tp_of_expr contexte b) args explist then tp else failwith "erreur arguments fonction"
                        with  Not_found -> failwith "fonction non trouvée")

let rec tp_cmd contexte = function 
    Skip -> VoidT
    |Exit -> VoidT
    |Assign (vname,exp) ->let t = tp_of_expr contexte exp in  VoidT 
    |Seq (com1,com2) -> let tp1 = tp_cmd contexte com1 and tp2 = tp_cmd contexte com2 in
                                tp2 
    |CondC (exp1, com1, com2) -> if tp_of_expr contexte exp1 = BoolT then 
        let tp1 = tp_cmd contexte com1 and tp2 = tp_cmd contexte com2 in
                                if tp1 = tp2 then tp1 else failwith "erreur branches de types différents"
            else failwith "Erreur: une condition doit être un booléen" 
    |Loop (com) -> tp_cmd contexte com
    |CallC (fname,explist) ->tp_of_expr contexte (CallE(fname, explist))
    |Return (exp) -> tp_of_expr contexte exp


let tp_prog (Prog (fundecls, fundefns)) = true
