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
                (match binop with
                    BArith b -> if b = BAadd || b = BAsub || b = BAmul || b= BAdiv ||b= BAmod then 
                            if tp1 = tp2 && tp1 = IntT then IntT else failwith "erreur opération entière"
                        else
                            if tp1 = tp2 && tp1 = FloatT then FloatT else failwith "erreur opération flotante"
                    |BBool b -> if tp1 = tp2 && tp1 = BoolT then BoolT else failwith "erreur opération booléenne"    
                    |BCompar b -> if tp1 = tp2 then BoolT else failwith "erreur opération comparaison")
    |CondE (expcond,exp1,exp2) -> let tpcond = tp_of_expr contexte expcond and tp1 = tp_of_expr contexte exp1 and tp2 = tp_of_expr contexte exp2 in
                                if tpcond = BoolT then
                                    if tp1 = tp2 then tp1 else failwith "les deux branches doivent avoir le même type"                           
                                else failwith "le type d'une condition doit être un booléen"
    |CallE (fname, explist) -> (try 
                            let Fundecl(tp, name, args) = List.find ((function fname -> function Fundecl(tp,name,args) -> name = fname) fname) contexte.funbind 
                            in
                            if List.length args = List.length explist && List.for_all2 (function Vardecl(tpa,aname) -> function exp -> tpa = tp_of_expr contexte exp) args explist then tp else failwith "erreur arguments fonction"
                        with  Not_found -> failwith (fname^" : fonction non trouvée"))

let rec tp_cmd contexte = function 
    Skip -> VoidT
    |Exit -> VoidT
    (* Assign ne se contente que de réassigner une valeur à une variable existante, elle ne permet pas d'en créer une*)
    |Assign (vname,exp) -> let tpe = tp_of_expr contexte exp in (try if tpe = (List.assoc vname contexte.localvars) then tpe else failwith ("Erreur : la variable "^vname^"n'est pas du type voulu") with Not_found -> failwith "Variable non déclarée" ) 
    |Seq (com1,com2) -> if tp_cmd contexte com1= VoidT then tp_cmd contexte com2 else failwith "c1 doit être de type VoidT"
    |CondC (exp1, com1, com2) -> if tp_of_expr contexte exp1 = BoolT then 
        let tp1 = tp_cmd contexte com1 and tp2 = tp_cmd contexte com2  in
                                if tp1 = tp2 then tp1 else failwith "erreur branches de types différents"
            else failwith "Erreur: une condition doit être un booléen" 
    |Loop (com) -> tp_cmd contexte com
    |CallC (fname,explist) ->tp_of_expr contexte (CallE(fname, explist))
    |Return (exp) -> tp_of_expr contexte exp

(*vérifier quet tp = tp com*)
let tp_fundefn contexte (Fundefn(Fundecl (tp,fname,arguments),com))=
    (*funUniq vérifie que cette fonction n'est pas déjà définie*)
    let rec funUniq contexte (Fundecl (tp,fname,arguments))= List.for_all (function Fundecl(_,fname2,_)-> not(fname=fname2)) contexte.funbind
    and
    (*argDiff vérifie que les noms des arguments sont disjoints*)
    argDiff (args)=
            match args with
            (Vardecl(_,vname)::q)-> List.for_all (function Vardecl(_,name2)-> not(vname=name2)) q && argDiff(q)
            | []-> true
    in
    if funUniq contexte (Fundecl(tp,fname,arguments)) && argDiff(arguments) then 
        (* création de l'environnement initial contenant tous les arguments, List.append pour le rajouter au contexte global*)
        let env= { localvars= (List.map (function Vardecl(tp,vname)->(vname,tp)) arguments)@ (contexte.localvars)  ;
            funbind= Fundecl (tp,fname,arguments) :: (contexte.funbind)}
        in
        tp_cmd env com
    else failwith("Certains arguments ont le même nom ou la fonction est déjà définie")

    
    
(* Fonction renvoyant vrai ssi le programme est correctement typé*)
let tp_prog (Prog (fundecls, fundefns)) = 
    let rec aux env = function
        |(Fundefn(Fundecl(tp,fname,ars),com)::l) -> (tp_fundefn env (Fundefn(Fundecl(tp,fname,ars),com)))= tp && aux {localvars = env.localvars; funbind = (Fundecl(tp,fname,ars)::(env.funbind))} l
        |[] -> true
    in aux {localvars = []; funbind = fundecls} fundefns
