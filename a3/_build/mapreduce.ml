(* 

Name: Chris Piller
Email: cpiller@princeton.edu
Minutes Spent on Problem 1.1: 30
Minutes Spent on Problem 1.2: 150

(You aren't in any way graded on the number of minutes spent; 
 we are just trying to calibrate for future versions of the class)

Comments/Problems/Thoughts on this part of the assignment:

*)

(* This part of the assignment uses the following functions 
 * If you forget how they work, look them up:
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *)

let map : ('a -> 'b) -> 'a list -> 'b list = List.map;;

let filter : ('a -> bool) -> 'a list -> 'a list = List.filter;;

let foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = List.fold_right;;

let foldl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b = List.fold_left;;

(* reduce is equivalent to List.fold_right, 
 * only its args are ordered differently *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl) ;;

(***********************************************)
(******            1.1 WARM UP            ******)
(***********************************************)

(* Solve each problem in this part using map, reduce, foldl, foldr or filter.
 * Map, filter, reduce, foldl, foldr are an example of a *combinator library*:
 * a library of higher-order functions used to solve problems in a particular
 * domain.  In this case, the domain is list-processing.  However, there are 
 * countless other useful combinator libraries.  The goal is to get used to 
 * thinking about how to decompose complex functions in to smaller, simpler, 
 * orthogonal functional components.  The smaller, simpler functional 
 * components can be constructed directly using the combinator library.
 *
 * Note: foldl is slightly more efficient than foldr because it is tail
 * recursive.  (We will explain what that means later in the course.)
 * Hence, solutions that use foldl are typically superior to solutions
 * that use foldr, all other things being equal.  Thus you should try to 
 * use foldl where you can instead of foldr (but take care to retain good 
 * style -- a horribly convoluted, incomprehensible function that uses 
 * foldl is worse than an elegant one that uses foldr).
 *
 * In these problems, you are not allowed to use the "rec" keyword in 
 * your solution.  A solution, even a working one, that uses explicit 
 * recursion via "rec" will receive little to no credit.  You may write
 * useful auxiliary functions; they just may not be recursive.
 *
 * You are also not allowed to use other functions from the list library
 * such as sort, concat or flatten.  (You are allowed to recode those
 * functions yourself using map, filter, fold if you find it necessary.)
 *
 *)

(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)
let negate_all (nums:int list) : int list =
  map (fun x -> (-1) * x) nums
;;

(* Unit test example.  Uncomment after writing negate_all *)
assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);;


(*>* Problem 1.1.b *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)
let sum_rows (rows:int list list) : int list =
  let row_sum row =
    reduce (fun x y -> x + y) 0 row
in
  map row_sum rows
;;

assert ((sum_rows [[1;2]; [3;4]]) = [3; 7]);;

(*>* Problem 1.1.c *>*)

(*  limit_range : Returns a list of numbers in the input list within a
 *                  given range (inclusive), in the same order they appeared
 *                  in the input list.
 *       Example : limit_range [1;3;4;5;2] (1,3) = [1;3;2] *)
let limit_range (nums:int list) (range:int * int) : int list =
  let (lower,upper) = range in
  let is_in_range x = 
    ((x>=lower) && (x<=upper))
  in
  filter is_in_range nums
;;

assert ((limit_range [1;3;4;5;2] (1,3)) = [1;3;2]);;

(*>* Problem 1.1.d *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)
let num_occurs (n:int) (nums:int list) : int =
  let increment_if_equal x y = 
    if x = n then 1 + y
  else y
in
  reduce increment_if_equal 0 nums
;;

assert ((num_occurs 4 [1;3;4;5;4]) = 2);;

(*>* Problem 1.1.e *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)
let super_sum (nlists:int list list) : int =
  let ls_sum xs y = 
    (reduce (fun x y -> x + y) 0 xs) + y
  in
  reduce ls_sum 0 nlists
;;

assert ((super_sum [[1;2;3];[];[5]]) = 11);;


(****************************************************)
(**********       1.2 A Bigger Challenge   **********)
(****************************************************)

(*
 * Note: some of these questions may be challenging.  
 * Don't neglect Part 2 of this assignment because you are working on
 * these problems.
 *)

(*>* Problem 1.2.a *>*)

(* min2: returns the second-smallest element of a list, when put into 
 * sorted order. Note that if list contains duplicates, the second-smallest 
 * element and the smallest element may be identical; your code should return 
 * it.
 *
 * Example: min2 [2110; 4820; 3110; 4120] = 3110.
 * Example: min2 [2110; 4820; 2110; 4120] = 2110.
 *
 * For full credit, use a fold function, do not sort the list and do not
 * use the rec keyword (aside from using folds). 
 *
 * You will receive partial credit if you use explicit recursion instead of
 * a fold.
 *
 * If the list contains 0 or 1 elements, call (invalid_arg s) with a helpful
 * string s. See the Pervasives library for the invalid_arg function and 
 * other useful exception-raising functions:
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
*)

let min2 (xs:int list) : int = 
  let length ls = 
    foldr (fun x y -> 1 + y) ls 0
  in 
  let xs_len = length xs in
  if ((xs_len = 0) || (xs_len = 1)) then invalid_arg "list to short for min2";
  let three_to_two_mins new_num (old_min, old_min2) =
    if (new_num < old_min) then (new_num, old_min)
    else if (new_num < old_min2) then (old_min, new_num)
    else (old_min, old_min2)
  in
  let (xs_min, xs_min2) =
    foldr three_to_two_mins xs (max_int, max_int)
  in
  xs_min2
;;

assert ((min2 [2110; 4820; 3110; 4120]) = 3110);;
assert ((min2 [2110; 4820; 2110; 4120]) = 2110);;

(*>* Problem 1.2.b *>*)

(* consec_dedupe : removes consecutive duplicate values from a list. 
 * More specifically, consec_dedupe has two arguments:
 *  eq is a function equiv representing an equivalence relation
 *  xs is a list of elements. 
 * It returns a list containing the same elements as lst, but without 
 * any duplicates, where two elements are considered equal if applying eq 
 * to them yields true.
 *
 * Example: consec_dedupe (=) [1; 1; 1; 3; 4; 4; 3] = [1; 3; 4; 3].
 *
 * Example: 
 *
 * let nocase_eq (s1:string) (s2:string) : bool =
 *   (String.uppercase s1) = (String.uppercase s2)
 * ;;
 * 
 * consec_dedupe nocase_eq ["hi"; "HI"; "bi"] = ["hi"; "bi"]
 *  
 * (When consecutive duplicates are not exactly syntactically equal
 * as above, it does not matter which of the duplicates are discarded.)
 *
 * Again, for full credit, do not use explicit recursion (the rec keyword),
 * but instead use foldr or foldl (or both).
 *
 * Partial credit will be given to solutions that do use explicit recursion.
 *)

let consec_dedupe (eq:'a -> 'a -> bool) (xs:'a list) : 'a list =
  let reverse_list ls =
    foldl (fun u x -> x::u) [] ls
  in
  let next_is_dup head tail =
    match tail with
    | [] -> false
    | tl_hd::tl_tl -> 
    if (eq head tl_hd) then true else false
  in
  let reverse_no_dupes ls = 
    let add_if_no_dupes u new_elem =
      if (next_is_dup new_elem u) then u else new_elem::u
    in
    foldl add_if_no_dupes [] ls
  in
  reverse_list (reverse_no_dupes xs)
;;

assert ((consec_dedupe (=) [1; 1; 1; 3; 4; 4; 3]) = [1; 3; 4; 3]);;

(*>* Problem 1.2.c *>*)

(* prefixes: return a list of all non-empty prefixes of a list, 
 * ordered from shortest to longest.

    Example: prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]].
*)

let prefixes (xs: 'a list) : 'a list list =
  let my_map f ls =
    foldr (fun x u -> (f x)::u) ls []
  in
  let reverse_list ls =
    foldl (fun u x -> x::u) [] ls
  in
  let add_next_elem u x =
    match u with 
    | [] -> (x::[])::u
    | hd::tl -> (x::hd)::u
  in
  let doubly_rev_prefixes = (foldl add_next_elem [] xs) in
  let single_rev_prefixes = my_map reverse_list doubly_rev_prefixes in
  reverse_list single_rev_prefixes
;;

assert ((prefixes [1;2;3;4]) = [[1]; [1;2]; [1;2;3]; [1;2;3;4]]);;

(*>* Problem 1.2.d *>*)

(* k_sublist : Given a list of integers nums and an integer k, 
 * the function k_sublist computes the contiguous sublist of length k 
 * whose elements have the largest sum.

    Example: k_sublist [1; 2; 4; 5; 8] 3 = [4; 5; 8].

 * raise invalid_arg "k_sublist" if the length of nums is less than k
 *)

(* assumes k is a natural number *)
let k_sublist (nums: int list) (k:int) : int list =
  let length ls = 
    foldr (fun x y -> 1 + y) ls 0
  in 
  (
    if (length nums < k) then invalid_arg ("k sublist");
    let my_map f ls =
      foldr (fun x u -> (f x)::u) ls []
    in
    let reverse_list ls =
      foldl (fun u x -> x::u) [] ls
    in
    let append_to_substrings u x =
      let append_if_small sub_u =
        if ((length sub_u) < k) then x::sub_u else sub_u
      in
      my_map append_if_small ([]::u)
    in
    let unpruned_sublists = foldl append_to_substrings [] nums in
    let append_if_k_len x u_ls = if (length x = k) then (x::u_ls) else u_ls in
    let all_k_sublists = foldr append_if_k_len unpruned_sublists [] in
    let larger_sublist xs1 xs2 =
      let xs1_sum = foldr (fun x y -> x + y) xs1 0 in
      let xs2_sum = foldr (fun x y -> x + y) xs2 0 in
      if (xs1_sum >= xs2_sum) then xs1 else xs2
    in
    reverse_list (foldr larger_sublist all_k_sublists [])
  )
;;

assert ((k_sublist [1; 2; 4; 5; 8] 3) = [4; 5; 8]);;

(*>* Problem 1.2.e *>*)
(* flatten : write a function that flattens a list of lists in to a single
 * list with all of the elements in order. eg:
 *
 * flatten [[1;2;3]; []; [0]; [4;5]] == [1;2;3;0;4;5] 
 *
 * In the last assignment you wrote this function with recursion. Now, 
 * do it without recursion, using folds.
 *)

let flatten (xss:'a list list) : 'a list =
  let join_lists ls1 ls2 = 
    foldr (fun y ys -> y::ys) ls1 ls2
  in
  foldr join_lists xss []
;;

assert ((flatten [[1;2;3]; []; [0]; [4;5]]) = [1;2;3;0;4;5]);;