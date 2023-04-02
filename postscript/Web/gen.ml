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

(* str_of_bop doit détailler chaque BinOp en commande correspondante de postScript*)