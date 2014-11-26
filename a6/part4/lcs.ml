open Memoizer
open Timing
open Random

type base = Base.base;;
type dna = Base.dna;;

(* slow lcs *)
let rec slow_lcs ((s1,s2) : dna * dna) : dna =
  match (s1,s2) with 
      ([], _) -> []
    | (_, []) -> []
    | (x :: xs, y :: ys) ->
      if Base.eq x y then
	x :: slow_lcs (xs, ys)
      else
	Base.longer_dna_of (slow_lcs (s1, ys)) (slow_lcs (xs, s2))
;;

(* A potentially useful module *)
module DnaPairOrder : Map.OrderedType with type t = dna * dna =
struct
    type t = dna * dna

    let rec compare_dna x' y' : int = 
        match x',y' with 
          [],[] -> 0
        | [], xs -> -1
        | xs, [] -> 1
        | x::xs, y::ys -> 
	  (match Base.compare x y with
	      0 -> compare_dna xs ys
            | other -> other)
	    

    (* implements a lexicographic ordering: 
     * compare the second components only if first components are equal *)
    let compare (a, b) (c, d) =
      match compare_dna a c with
	  0 -> compare_dna b d
        | other -> other
     
end;;

(* Task 4.4 *)

(* implement fast_lcs using your automatic memoizer functor! 
 * doing so will of course require proper creation of modules and
 * use of functors *)
module D = Map.Make(DnaPairOrder);;
module M = Memoizer(D);;

let fast_lcs (ds : dna * dna) : dna =
  let lcs_stepwise recurse (s1,s2) =
    match (s1,s2) with 
      | ([], _) -> []
      | (_, []) -> []
      | (x :: xs, y :: ys) ->
        if Base.eq x y then
        x :: recurse (xs, ys)
        else
        Base.longer_dna_of (recurse (s1, ys)) (recurse (xs, s2))
  in
  (M.memo lcs_stepwise) ds
;;

(* Task 4.5 *)

(* Implement some experiment that shows performance difference
 * between slow_lcs and fast_lcs. (Print your results.)     
 * Explain in a brief comment what your experiment shows.        *)
let print_header () =
  print_string "------------ ------- LCS -------------\n";
  print_string "    Lengths     Slow     Fast \n";
  print_string "------------ ----------------------------\n"
;;
let print_row n slow fast =
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
  print_newline()
;;
let random_base () : base =
  match Random.int 4 with 
  | 0 -> A
  | 1 -> T
  | 2 -> C
  | 3 -> G
  | _ -> failwith "impossible"
;;
let rec random_ds length = 
  if length <= 0 then ([], [])
  else 
    let (s1,s2) = random_ds (length-1) in
    ((random_base ())::s1,(random_base ())::s2)
;;
let experiment (len:int) : unit =
  let ds = random_ds len in
  let slow n = if len > 42 then None else Some (time_fun slow_lcs ds) in
  let fast = 
    time_fun fast_lcs
  in
  print_string ((Base.dna_to_string (slow_lcs ds))^"\n");
  print_string ((Base.dna_to_string (fast_lcs ds))^"\n");
  print_row len (slow ds) (fast ds)
;;
let test_random_ds () = 
  let (s1,s2) = (random_ds 10) in
  print_string ((Base.dna_to_string s1)^"   "^(Base.dna_to_string s2))
;;
let main () =
  (* This test creates random dna of a certain length and then tests how
    long each method takes to compute the lcs.  Most of the printing was
    copied from fib.ml. This also prints the results of the functions,
    which makes the execution twice as slow but ensures the fast function
    works.*)
  let trials = [1;2;4;8;16;17;18;19;20] in
  print_header();
  List.iter experiment trials
;;


(* uncomment this block to run your experiment, 
 * but please do not submit with it uncommented
 *)
(* main ();; *)