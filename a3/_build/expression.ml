(* 

Name:
Email:
Minutes Spent on Problem 2:

(You aren't in any way graded on the number of minutes spent; 
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:

*)

open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry 
 *    about expressionlibrary.ml
 * 3. Test!  (Use "assert" where appropriate.)
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
  match e with
  | Num n -> false
  | Var -> true
  | Unop (u,e1) -> (contains_var e1)
  | Binop (b,e1,e2) -> (contains_var e1) || (contains_var e2)
;;

assert (contains_var (parse "x^4"));;
assert (not (contains_var (parse "4+3")));;

(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Use OCaml's
 *            built in method of handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  | Num n -> n
  | Var -> x
  | Unop (u,e1) -> (
    match u with
    | Sin -> sin (evaluate e1 x)
    | Cos -> cos (evaluate e1 x)
    | Ln -> log (evaluate e1 x)
    | Neg -> (-1.) *. (evaluate e1 x))
  | Binop (b,e1,e2) -> (
    match b with
    | Add -> (evaluate e1 x) +. (evaluate e2 x)
    | Sub -> (evaluate e1 x) -. (evaluate e2 x)
    | Mul -> (evaluate e1 x) *. (evaluate e2 x)
    | Div -> (evaluate e1 x) /. (evaluate e2 x)
    | Pow -> (evaluate e1 x) ** (evaluate e2 x))
;;

assert ((evaluate (parse "x^4 + 3") 2.0) = 19.0);;

(*>* Problem 2.3 *>*)

(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with
  | Num n -> 0
  | Var -> 1
  | Unop (u,e1) -> (
    match u with
    | Sin -> sin (derivative e1)
    | Cos -> cos (derivative e1)
    | Ln -> log (derivative e1)
    | Neg -> (-1.) *. (derivative e1))
  | Binop (b,e1,e2) -> (
    match b with
    | Add -> Binop(Add,(derivative e1),(derivative e2))
    | Sub -> Binop(Sub,(derivative e1),(derivative e2))
    | Mul -> Binop(Add, Binop(Mul,(derivative e1),e2), Binop(Mul,e1,(derivative e2)))
    | Div -> Binop(Div, 
      Binop(Sub, Binop(Mul,(derivative e1),e2), Binop(Mul,e1,(derivative e2))), 
      Binop(Pow, e2, 2))
    | Pow -> 
      match e2 with
      | Num n -> 
      | _ ->
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval=
  print_string ("Checking expression: " ^ strs^"\n");
  let parsed = parse strs in (
        print_string "contains variable : ";
	print_string (string_of_bool (contains_var parsed));
	print_endline " ";
	print_string "Result of evaluation: ";
	print_float  (evaluate parsed xval);
	print_endline " ";
	print_string "Result of derivative: ";
	print_endline " ";
	print_string (to_string (derivative parsed));
	print_endline " ");;


(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  failwith "Not implemented" 
;;



(*>* Problem 2.5 *>*)

(* See writeup for instructions. *)
let rec find_zero_exact (e:expression) : expression option =
  failwith "Not implemented" 
;;


(*>* Problem 2.6 *>*)

(* Only adds parentheses when needed to prevent ambiguity. *)
(* See observations in the writeup. *)
let to_string_smart (e:expression) : string =
  failwith "Not implemented"
;;

