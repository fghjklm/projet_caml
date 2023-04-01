(* Compilation functions *)

open Lang
open Instrs
open Typing

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)


let gen_prog (Prog (fundecls, fundefns)) =
  ISeq []

(*
let rec gen_expr = function
  Const c-> IVal(c)
  |BinOp (bop,expr1,expr2)->
    let str_of_bop= function
    BArith ar-> (match ar with
                  BAadd-> "add")
    (*|BBool bo->
    |BCompar co->*)
    in
    ISeq([gen_expr(expr1);gen_expr(expr2);IOp(str_of_bop(bop))])
    
*)
(* str_of_bop doit détailler chaque BinOp en commande correspondante de postScript*)