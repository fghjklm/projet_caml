(* Compilation functions *)

open Lang
open Instrs
open Typing

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)

let rec lookup_index v = function
   | [] -> failwith ("unable to find variable " ^ v)
   | (vn,t) :: locs -> if v = vn then 0 else 1 + lookup_index v locs

let rec popL = function
  (a::q) -> (IOper "pop")::(popL q)
  |[] -> []

let rec gen_expr offset env= function
  Const c-> IVal(c)
  |BinOp (bop,expr1,expr2)->
    (* str_of_bop doit détailler chaque BinOp en commande correspondante de postScript*)
    let str_of_bop= (function
    BArith ar-> (match ar with
                  BAadd-> "add"
                  |BAfadd -> "add"
                  |BAsub -> "sub"
                  |BAfsub-> "sub"
                  |BAdiv -> "idiv"
                  |BAfdiv -> "div"
                  |BAmod -> "mod"
                  |BAmul -> "mul"
                  |BAfmul -> "mul")
    |BBool bo-> if bo = BBand then "and" else "or"
    |BCompar co->(match co with 
        BCeq -> "eq"
        |BCge -> "ge"
        |BCgt -> "gt"
        |BCle -> "le"
        |BClt -> "lt"
        |BCne -> "ne"))
    in
    ISeq([gen_expr offset env expr1;gen_expr (offset+1) env expr2 ;IOper(str_of_bop(bop))])
  |CondE(expcond, exp1, exp2) -> ISeq[gen_expr offset env (expcond); IBloc(gen_expr (offset+1) env exp1); IBloc(gen_expr (offset +1) env (exp2)); IOper("ifelse")]
  |CallE(fname, explist) ->
     let rec auxCallE explist offset env = (match explist with
        (a::q) -> (gen_expr offset env a) ::(auxCallE q (offset+1) env)
        |[] -> [])
    in  ISeq ((auxCallE explist offset env)@[IOper(fname)])
  |VarE(vname) -> IVar ((lookup_index vname env.localvars) + offset)





    
let rec gen_cmd env  = function
  Skip -> ISeq []
  |Exit -> IOper "exit"
  | Assign(v, e) ->
    let i = lookup_index v env.localvars in
    let roll_up = [IVal (IntV (i + 2)); IVal (IntV (-1)); IOper "roll"] in
    let roll_down = [IVal (IntV (i + 1)); IVal (IntV 1); IOper "roll"] in
    ISeq ((gen_expr 0 env e) :: roll_up @ [IOper "pop"] @ roll_down)

  |Seq(com1,com2) -> ISeq[gen_cmd env com1; gen_cmd env com2]
  |CondC(expcond, com1, com2) -> ISeq[gen_expr 0 env (expcond); IBloc(gen_cmd  env com1); IBloc(gen_cmd env com2); IOper("ifelse")]
  |Loop(com) -> ILoop(gen_cmd env com)
  |CallC(fname, explist) -> ISeq ((List.mapi (fun i -> gen_expr i env ) explist)@ [IOper fname])
  |Return (exp) -> gen_expr 0 env exp

   
let gen_fun (Fundefn(Fundecl(tp,fname,varl), com)) env= 
    let env1 = {funbind = (Fundecl(tp,fname,varl))::(env.funbind); localvars = List.rev(List.map(function Vardecl(tp,vname) -> (vname,tp)) varl)}
    in (IDef(fname, ISeq([gen_cmd env1 com]@(popL(varl)))),env1)

let gen_prog (Prog (fundecls, fundefns)) =
    let env = {localvars = []; funbind = fundecls} in 
    let rec fundefnsToInstr env = function
        (a::q) -> let (instr,env1m) = gen_fun a env in instr::(fundefnsToInstr env q)
        |[] -> []
in ISeq (fundefnsToInstr env fundefns)

    