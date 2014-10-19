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

let trim_env (env:env) (top_e:exp) (ignore_ls:variable list) : env =
  let join_lists ls1 ls2 = List.fold_right (fun y ys -> y::ys) ls1 ls2 in
  let rec contains ls item =
    match ls with
    | [] -> false
    | hd::tl -> if (hd = item) then true else (contains tl item)
  in
  let rec no_dups ls = 
    match ls with
    | [] -> []
    | hd::tl -> if contains tl hd then no_dups tl else hd::(no_dups tl)
  in
  let rec free_vars bound_env free e =
    match e with
    | Var x -> (match lookup_env bound_env x with 
       | None -> x::free
       | Some v -> free)
    | Constant _ -> free  
    | Op (e1,op,e2) -> 
      join_lists (free_vars bound_env free e1) (free_vars bound_env free e2)
    | If (e1,e2,e3) -> 
      join_lists
        (join_lists 
          (free_vars bound_env free e1)
          (free_vars bound_env free e2))
        (free_vars bound_env free e3)
    | Let (x,e1,e2) ->
      let new_bound_env = update_env bound_env x e1 in
      (join_lists 
        (free_vars bound_env free e1) 
        (free_vars new_bound_env free e2))
    | Pair (e1,e2) ->
      (join_lists (free_vars bound_env free e1) (free_vars bound_env free e2))
    | Fst e1 -> free_vars bound_env free e1
    | Snd e1 -> free_vars bound_env free e1
    | EmptyList -> free
    | Cons(e1,e2) -> 
      (join_lists 
        (free_vars bound_env free e1) 
        (free_vars bound_env free e2))
    | Match(e1,e2,hd,tl,e3) -> 
      let new_bound_env_hd = (update_env bound_env hd EmptyList) in
      let new_bound_env = (update_env new_bound_env_hd tl EmptyList) in
      join_lists
        (join_lists 
          (free_vars bound_env free e1) 
          (free_vars bound_env free e2))
        (free_vars bound_env free e3)
    | Rec (f,x,body) -> 
      let new_bound_env_f = (update_env bound_env f EmptyList) in
      let new_bound_env = (update_env new_bound_env_f x EmptyList) in
      join_lists (free_vars bound_env free e1) (free_vars bound_env free e2)
    | App (e1,e2) -> 
      (join_lists 
        (free_vars bound_env free e1) 
        (free_vars new_bound_env free e2))
    | Closure (c_env,f,x,body) -> 
      let new_bound_env_f = (update_env bound_env f EmptyList) in
      let new_bound_env = (update_env new_bound_env_f x EmptyList) in
      free_vars new_bound_env free body
  in
  let free_ls = free_vars empty_env [] top_e in
  let no_dups_free_ls = no_dups free_ls in
  let rec make_pruned_env frees = 
    match frees with
    | [] -> []
    | hd::tl -> if contains ignore_ls hd then (make_pruned_env tl) else
      match lookup_env env hd with
      | None -> raise (UnboundVariable hd)
      | Some v -> (hd,v)::(make_pruned_env tl)
  in
  make_pruned_env no_dups_free_ls
;;

(* evaluation; use eval_loop to recursively evaluate subexpressions *)
let eval_body (env:env) (eval_loop:env -> exp -> exp) (e:exp) : exp = 
  if is_value e then e else
  match e with
    | Var x -> 
      (match lookup_env env x with 
  	   | None -> raise (UnboundVariable x)
	     | Some v -> v)
    | Constant _ -> e  
    | Op (e1,op,e2) -> 
        let v1 = eval_loop env e1 in 
        let v2 = eval_loop env e2 in 
          apply_op v1 op v2 
    | If (e1,e2,e3) -> 
          (match eval_loop env e1 with 
             | Constant (Bool true) -> eval_loop env e2
             | Constant (Bool false) -> eval_loop env e3
             | v1 -> raise (BadIf v1))
    | Let (x,e1,e2) -> eval_loop (update_env env x (eval_loop env e1)) e2

    | Pair (e1,e2) ->  Pair(eval_loop env e1, eval_loop env e2)
    | Fst e1 -> (
      match e1 with
      | Pair (efst, esnd) -> eval_loop env efst
      | v1 -> raise (BadPair v1))
    | Snd e1 ->  (
      match e1 with
      | Pair (efst, esnd) -> eval_loop env esnd
      | v1 -> raise (BadPair v1))
    | EmptyList -> EmptyList
    | Cons(e1,e2) -> Cons((eval_loop env e1),(eval_loop env e2))
    | Match(e1,e2,hd,tl,e3) -> (
      match (eval_loop env e1) with 
      | EmptyList -> (eval_loop env e2)
      | Cons(hdexp,tlexp) -> 
        let sub_env = update_env (update_env env hd hdexp) tl tlexp in
        eval_loop sub_env e3
      | v1 -> raise (BadMatch v1))
    | Rec (f,x,body) -> 
      let ignore = [f; x] in
      Closure(trim_env env body ignore,f,x,body)
    | App (e1,e2) -> 
        (
          match eval_loop env e1 with 
          | Closure (c_env,f,x,body) -> 
            let app_c_env = update_env c_env f (eval_loop c_env body) in
            let app_c_env_2 = update_env app_c_env x (eval_loop env e2) in
            eval_loop app_c_env_2 body
          | v1 -> raise (BadApplication v1)
      )
    | Closure (c_env,f,x,body)  -> Closure (c_env body,f,x,body) 

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
