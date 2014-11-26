open Memoizer
open Timing

(* See http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Map.html 
 * http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Map.S.html
 * http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Map.OrderedType.html
 *)
module type DICT = Map.S;;
module type ORDERED = Map.OrderedType;;

module type FIB =
sig
  (* on input n, computes the nth Fibonacci number *)
  val fib : int -> int
end;;

(* an exponential time implementation of FIB.  Very slow! *)
module Fib : FIB =
struct
  let rec fib (n : int) : int =
    if n > 1 then 
      fib (n-1) + fib (n-2)
    else 
      n 
end;;

(* fast fib *)
module FastFib : FIB =
struct
  let fib (n : int) : int =
    let rec aux i f1 f2 = 
      if i = n then
	f1 + f2
      else
	aux (i+1) (f1+f2) f1
    in
    if n > 1 then aux 2 1 0
    else n	
end;;

module IntOrder : (ORDERED with type t = int) = 
struct
  type t = int
  let compare n1 n2 =
    if n1 = n2 then 0
    else if n1 < n2 then -1
    else 1
end;;

(* Task 4.1:                              *
 * implementation of FIB memoized by hand *)
module MemoFib (D : DICT with type key = int) : FIB =
struct

  let history = ref D.empty

  let rec fib n =
    match get_mem_val n with
    | None -> (
      let result = calculate_fib n in (
        store_val n result;
        result
      )
    )
    | Some value -> value

  and get_mem_val n = 
    if D.mem n !history then ( Some (D.find n !history))
    else None
 
  and calculate_fib n = 
    (if n < 0 then failwith "Can't handle negative Fibonacci args");
    if n < 2 then 1 
    else fib (n-1) + fib (n-2)

  and store_val n result =
    history := D.add n result !history

end;;

module ManualMemoedFib = MemoFib(Map.Make(IntOrder));;

(* Task 4.3:                             *
 * implementation of FIB memoized using  *
 *   Memoizer from memoizer.ml           *
 *   Map.Make to create a dictionary     *
 *   IntOrder from above                 *)
module AutoMemoedFib : FIB =
struct
  
  module D = Map.Make(IntOrder)

  module M = Memoizer(D)

  let fib_stepwise (recurse:int->int) (n:int) : int =
    (if n < 0 then failwith "Can't handle negative Fibonacci args");
    if n < 2 then 1 
    else recurse (n-1) + recurse (n-2)

  let fib = M.memo fib_stepwise

end;;

(* main function/testing *)

let print_header () =
  print_string "--------- --------------- Fib(N) -------------\n";
  print_string "    N     Slow     Fast     Manual   Automated\n";
  print_string "--------- ------------------------------------\n"
;;

let print_row n slow fast manual automated =
  let space () = print_string "   " in
  let float f = Printf.printf "%6.4f" f in
  let print_slow slow =
    match slow with
	None -> print_string "   -  " 
      | Some f -> float f
  in
  if n < 10 then print_string " ";
  if n < 100 then print_string " ";
  if n < 1000 then print_string " ";
  if n < 10000 then print_string " ";
  if n < 100000 then print_string " ";
  if n < 1000000 then print_string " ";
  print_int n; space ();
  print_slow slow; space ();
  float fast; space ();
  float manual; space ();
  float automated; space ();
  print_newline()
;;

let experiment (n:int) : unit =
  let slow n = if n > 42 then None else Some (time_fun Fib.fib n) in
  let fast = time_fun FastFib.fib  in   
  let manual = 
    time_fun ManualMemoedFib.fib
  in   
  let automated = 
    time_fun AutoMemoedFib.fib
  in  
  print_row n (slow n) (fast n) (manual n) (automated n)
;;

let main () =
  (* change these numbers if you want depending on the speed of your machine *)
  (* on my machine slow_fib starts taking visible time at input 30 *)
  let trials = [0;1;2;10;20;30;36;37;38;39;40;
		50;100;25000;50000;100000;200000] in
  print_header();
  List.iter experiment trials
;;



(* uncomment this block to run tests, 
 * but please do not submit with it uncommented
 *)
(*
main ();;
*)