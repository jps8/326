(*************************************************)
(* An environment-based evaluator for Dynamic ML *)
(*************************************************)

open Syntax
open Printing
open EvalUtil

(* Defines the subset of expressions considered values
   Notice that closures are values but the rec form is not -- this is
   slightly different from the way values are defined in the 
   eval_loopitution-based interpreter.  Rhetorical question:  Why is that?
   Notice also that Cons(v1,v2) is a value (if v1 and v2 are both values).
*) 
let rec is_value (e:exp) : bool = 
  match e with
      Constant _ -> true  
    | Pair (e1, e2) -> is_value e1 && is_value e2
    | EmptyList -> true
    | Cons (e1, e2) -> is_value e1 && is_value e2
    | Closure _ -> true
    | _ -> false

(*(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp = 
  match e with
    | Var x -> 
      (match lookup_env env x with 
  	   | None -> raise (UnboundVariable x)
	     | Some v -> v)
    | Constant _ -> e
    | Op (e1,op,e2) -> Op(eval_loop env e1,op,eval_loop env e2)
    | If (e1,e2,e3) -> If(eval_loop env e1,eval_loop env e2,eval_loop env e3)
    | Let (y,e1,e2) -> (* TODO: add to env *)(*)
        Let (y, eval_loop env e1, if x = y then e2 else eval_loop env e2)
    | Pair (e1,e2) -> Pair(eval_loop env e1, eval_loop env e2)
    | Fst e1 -> Fst (eval_loop env e1)
    | Snd e1 -> Snd (eval_loop env e1)
    | EmptyList -> EmptyList
    | Cons (e1,e2) -> Cons (eval_loop env e1, eval_loop env e2)
    | Match (e1,e2,hd,tl,e3) -> 
      Match (eval_loop env e1, eval_loop env e2, hd, tl,
             if x = hd || x = tl then e3 else eval_loop env e3)
    | Rec (f,y,e1) -> if x = f || x = y then e else Rec (f, y, eval_loop env e1)
    | App (e1,e2) -> App(eval_loop env e1,eval_loop env e2)
    | Closure _ -> raise (NoClosures e) 

(* evaluate closed, top-level expression e *)

let eval e =
  let rec loop env e = eval_body env loop e in
  loop empty_env e


(* print out subexpression after each step of evaluation *)
let debug_eval e = 
  let rec loop env e =
    if is_value e then e  (* don't print values *)
    else 
      begin
	Printf.printf "Evaluating %s\n" (string_of_exp e); 
	let v = eval_body env loop e in 
	Printf.printf 
	  "%s evaluated to %s\n" (string_of_exp e) (string_of_exp v); 
	v
      end
  in
  loop empty_env e
