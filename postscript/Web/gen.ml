(* Compilation functions *)

open Lang
open Instrs
open Typing

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)


let gen_prog (Prog (fundecls, fundefns)) =
  ISeq []


let rec gen_expr = function
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
    ISeq([gen_expr(expr1);gen_expr(expr2);IOper(str_of_bop(bop))])
  |CondE(expcond, exp1, exp2) -> ISeq[gen_expr(expcond); IBloc(gen_expr(exp1)); IBloc(gen_expr(exp2)); IOper("ifelse")]
  |CallE(fname, explist) -> ISeq ((List.map gen_expr explist)@[IOper(fname)])
  (* VarE ?????*)

let rec gen_cmd = function
    Skip -> IOper ""
    |Exit -> IOper "exit"
    |Assign(vname, exp) -> IDef (vname, gen_expr exp)
    |Seq(com1,com2) -> ISeq[gen_cmd com1; gen_cmd com2]
    |CondC(expcond, com1, com2) -> ISeq[gen_expr(expcond); IBloc(gen_cmd(com1)); IBloc(gen_cmd(com2)); IOper("ifelse")]
    |Loop(com) -> ILoop(gen_cmd com)
    |CallC(fname, explist) -> gen_expr (CallE(fname, explist))
    (**|Return (exp) ->*) 


    