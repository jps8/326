(*********************)
(* Dynamic ML Syntax *)
(*********************)

type variable = string 

(* Equality and Inequality for variables *)
let var_eq x y = (String.compare x y = 0)
let var_neq x y = not (String.compare x y = 0)

type constant = Int of int | Bool of bool 

type operator = Plus | Minus | Times | Div | Less | LessEq 

(* Match (e1, e2, hd, tl, e3) is a match statement with the following form:
 
   match e1 with 
     [] -> e2 
   | hd::tl -> e3 

   Closure (env, f, x, body) is a closure for a recursive function.
   The closure environment is env.  The recursive function is named f
   and x is the name of the parameter.  body is the body of the expression,
   and may contain f and x.

*)

type exp = 

  (* Basic *)
  | Var of variable   
  | Constant of constant
  | Op of exp * operator * exp
  | If of exp * exp * exp
  | Let of variable * exp * exp

  (* Pairs *)
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp

  (* Lists *)
  | EmptyList
  | Cons of exp * exp  
  | Match of exp * exp * variable * variable * exp  

  (* Recursive functions *)
  | Rec of variable * variable * exp
  | Closure of env * variable * variable * exp 
  | App of exp * exp

and env = (variable * exp) list

(*****************************)
(* Manipulating environments *)
(*****************************)
 
(* empty environment *)
let empty_env : env = []

(* lookup_env env x == Some v 
 *   where (x,v) is the most recently added pair (x,v) containing x
 * lookup_env env x == None 
 *   if x does not appear in env *)
let rec lookup_env (env:env) (x:variable) : exp option =
  match env with 
  | [] -> None
  | (hd_var, hd_exp)::tl -> 
    if (hd_var = x) then Some hd_exp
    else (lookup_env tl x)
;;

(* update env x v returns a new env containing the pair (x,v) *)
let update_env (env:env) (x:variable) (v:exp) : env = 
  let rec update_var environment updated =
    match environment with
    | [] -> ([], updated)
    | (hd_var,hd_exp)::tl ->
      if ((hd_var = x) && (not updated)) then (
        let (remaining,_) = update_var tl updated in
        ((x,v)::remaining, true))
      else (
        let (remaining,_) = update_var tl updated in
        ((hd_var,hd_exp)::remaining, updated))
  in
  let (new_env, updated) = update_var env false in
  if (updated) then new_env else (x,v)::env
;;

let string_of_const c = 
  match c with 
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b


let string_of_op op = 
  match op with 
    | Plus -> "+" 
    | Minus -> "-" 
    | Times -> "*" 
    | Div -> "/" 
    | Less -> "<" 
    | LessEq -> "<=" 

let max_prec = 10

let precedence e = 
  match e with 
    | Constant _ -> 0
    | Var _ -> 0
    | Op (_,Plus,_) -> 5
    | Op (_,Minus,_) -> 5
    | Op (_,Times,_) -> 3
    | Op (_,Div,_) -> 3
    | Op (_,Less,_) -> 7
    | Op (_,LessEq,_) -> 7
    | Let _ -> max_prec
    | If _ -> max_prec

    | Pair _ -> 0
    | Fst _ -> 2
    | Snd _ -> 2

    | EmptyList -> 0
    | Cons _ -> 8
    | Match _ -> max_prec

    | Rec _ -> max_prec
    | Closure _ -> max_prec
    | App _ ->  2

let rec env2string env =
  let elem2string x v = x ^ "=" ^ exp2string max_prec v in
  let rec aux env =
    match env with
  [] -> ""
      | [(x,v)] -> elem2string x v
      | (x,v)::rest -> elem2string x v ^ ";" ^ aux rest 
  in
  "[" ^ aux env ^ "]"

and exp2string prec e = 
  let p = precedence e in 
  let s = 
    match e with 
      | Constant c -> string_of_const c
      | Op (e1,op,e2) -> 
          (exp2string p e1) ^ " "^(string_of_op op)^" "^(exp2string prec e2)
      | Var x -> x
      | If (e1, e2, e3) -> 
        "if " ^ (exp2string max_prec e1) ^ 
        " then " ^ (exp2string max_prec e2) ^ 
        " else " ^ (exp2string p e3)
      | Let (x,e1,e2) -> "let "^x^" = "^(exp2string max_prec e1)^" in "^
          (exp2string prec e2)

      | Pair (e1, e2) -> 
    "(" ^ (exp2string max_prec e1) ^ "," ^ (exp2string max_prec e2)  ^ ")"
      | Fst e1 ->  "fst " ^ (exp2string p e1)
      | Snd e1 ->  "snd " ^ (exp2string p e1)

      | EmptyList -> "[]"
      | Cons (e1,e2) -> (exp2string p e1) ^ "::" ^ (exp2string prec e2) 
      | Match (e1,e2,hd,tl,e3) -> 
    "match " ^ (exp2string max_prec e1) ^ 
      " with [] -> " ^ (exp2string max_prec e2) ^ 
            " | " ^ hd ^ "::" ^ tl ^ " -> " ^ (exp2string p e3)

      | Rec (f,x,body) -> "rec "^f^" "^x^" = "^(exp2string max_prec body)
      | Closure (env,f,x,body) -> 
    "closure "^env2string env^" "^f^" "^x^" = "^(exp2string max_prec body)
      | App (e1,e2) -> (exp2string p e1)^" "^(exp2string p e2)

  in 
    if p > prec then "(" ^ s ^ ")" else s

let string_of_exp e = exp2string max_prec e 
let string_of_env env = env2string env

let test_update =
  let env = empty_env in
  let first = update_env env ("myX") (Var("3z")) in (
  print_string (string_of_env first);
  (let second = lookup_env first "myX" in
  match  second with
  | Some v -> print_string (string_of_exp v)
  | _ -> ());
  let third = update_env first ("myX") (Var("18z")) in (
  print_string (string_of_env third);)
)
;;

test_update;;
