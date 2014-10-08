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
    | Div -> (evaluate e1 x) /. (evaluate e2 x) (*lets ocaml handle e1/0 errors*)
    | Pow -> (evaluate e1 x) ** (evaluate e2 x))
;;

assert ((evaluate (parse "x^4 + 3") 2.0) = 19.0);;

(*>* Problem 2.3 *>*)

(* See writeup for instructions.  *)
let rec derivative (e:expression) : expression =
  match e with
  | Num n -> Num 0.
  | Var -> Num 1.
  | Unop (u,e1) -> (
    match u with
    | Sin -> Binop(Mul, (derivative e1), Unop(Cos, e1))
    | Cos -> Binop(Mul, (derivative e1), Unop(Neg, Unop(Sin, e1)))
    | Ln -> Binop(Div, (derivative e1), e1)
    | Neg -> Binop(Mul, (Num (-1.)), (derivative e1))
  )
  | Binop (b,e1,e2) -> (
    match b with
    | Add -> Binop(Add,(derivative e1),(derivative e2))
    | Sub -> Binop(Sub,(derivative e1),(derivative e2))
    | Mul -> Binop(Add, Binop(Mul,(derivative e1),e2), Binop(Mul,e1,(derivative e2)))
    | Div -> Binop(Div, 
      Binop(Sub, Binop(Mul,(derivative e1),e2), Binop(Mul,e1,(derivative e2))), 
      Binop(Pow, e2, Num 2.))
    | Pow -> (
      match e2 with
      | Num h -> Binop(Mul, Num h, Binop(Mul, (derivative e1), Binop(Pow, e1, Binop(Sub, Num h, Num 1.))))
      | _ -> Binop(Mul,
        Binop(Pow, e1, e2),
        Binop(Add,
          Binop(Mul, derivative e2, Unop(Ln, e1)),
          Binop(Div, Binop(Sub, derivative e1, e2), e1)
        )
      )
    )
  )
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
	print_endline " ")
;;

(* derivative testing *)
(* checkexp (to_string (make_exp 4)) 5.;; *)

(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  if (lim <= 0) then None else
  let next_approx = g -. (evaluate (derivative e) g)/.(evaluate e g) in
  (
    if ((evaluate e next_approx < epsilon) 
      && (evaluate e next_approx > (-1.)*.epsilon)) then Some next_approx else
    find_zero e next_approx epsilon (lim-1)
  )
;;

let test_find_zero = 
  let eps = 0.1 in
  let test_zero = (find_zero (parse "x^2") 10. eps 100) in
  match test_zero with
  | None -> assert false
  | Some x ->
  assert ((x < eps) && (x > (-1.)*.eps))
;;
(* test_find_zero;; *)

(*>* Problem 2.5 *>*)

(* See writeup for instructions. *)
let rec find_zero_exact (e:expression) : expression option =
  let rec axplusb_form e_uncompressed =
    match e_uncompressed with
    | Num n -> Some (Num 0., Num n)
    | Var -> Some (Num 1., Num 0.)
    | Unop (u,e1) -> (
      match u with
      | Sin -> None
      | Cos -> None
      | Ln -> None
      | Neg -> 
        match (axplusb_form e1) with 
        | None -> None
        | Some (a,b) -> Some (Unop(Neg, a), Unop(Neg, b))
    )
    | Binop (b,e1,e2) -> (
      match b with
      | Add -> (
        match (axplusb_form e1) with
        | None -> None
        | Some (a1, b1) -> (
          match (axplusb_form e2) with
          | None -> None
          | Some (a2, b2) -> Some (Binop(Add, a1, a2), Binop(Add, b1, b2))
        )
      )
      | Sub -> (
        match (axplusb_form e1) with
        | None -> None
        | Some (a1, b1) -> (
          match (axplusb_form e2) with
          | None -> None
          | Some (a2, b2) -> Some (Binop(Sub, a1, a2), Binop(Sub, b1, b2))
        )
      )
      | Mul -> (
        match (axplusb_form e1) with
        | None -> None
        | Some (a1, b1) -> (
          match (axplusb_form e2) with
          | None -> None
          | Some (a2, b2) -> 
            if ((contains_var e1) && (contains_var e2)) then None
            else Some (
              Binop(Add, 
                Binop(Mul, a1, b2),
                Binop(Mul, a2, b1)
              ),
              Binop(Mul, b1, b2)
            )
        )
      )
      | Div -> None
      | Pow -> None
      )
  in
  match axplusb_form e with
  | None -> None
  | Some (a,b) ->
    if ((evaluate a 0.) = 0.) then None
    else Some (Unop(Neg, Binop(Div, b, a)))
;;

let exact_test = 
 match (find_zero_exact (parse "(x + 5) + (3*x + 3)")) with
  | Some value ->
  print_string ((to_string (value)) ^ "\n")
  | _ -> print_string "broke\n"
;;
exact_test;;

(*>* Problem 2.6 *>*)

(* Only adds parentheses when needed to prevent ambiguity. *)
(* See observations in the writeup. *)
let to_string_smart (e:expression) : string =
  failwith "Not implemented"
;;

