(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in a comment why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
 *)

(* Problem 1a - fix the right-hand-side of the assignment to match the
   listed type. (Do not change the left-hand-side.)

   Did not typecheck because: commas are used for tuples, not lists.


 *)

let prob1a : int list = [1; 2; 3] ;;


(* Problem 1b - fix the type of variable prob1b to match the type of
   the expression on the right-hand-side of the assignment.

   Did not typecheck because: it is a list of string * int tuples,
   not a tuple of string * int list.
 *)

let prob1b : (string * int) list = [("COS", 326); ("COS", 441)] ;;


(* Problem 1c - fix the right-hand-side of the expression to match 
   the variable prob1c's listed type.
   
   Did not typecheck because: the expression to the right of the ::
      must be a list of the expression to the left

    *)

let prob1c : float list = 2.0 :: (3.0 :: [4.0; 5.0]) ;;


(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types: 
 *
 * NOTE: for option and list types, you must go at least one layer deep.
 * example problems:
 *   let x : int option = ??? ;;
 *   let y : int list = ??? ;;
 * incorrect answers:
 *   let x : int option = None ;;
 *   let y : int list = [] ;;
 * possible correct answers:
 *   let x : int option = Some 1 ;;
 *   let y : int list = [1] ;;
 *   let y : int list = [1; 2] ;;
 *   let y : int list = 1 :: [] ;;
*)

(*>* Problem 2a *>*)

let prob2a : (float * (string * int) option list) list =
  [(5.0, [Some ("Hello World", 5)])]
;;


(*>* Problem 2b *>*)
(* a student is a (name, age option) pair *)

type student = string * int option;;


let prob2b : (student list option * int) list = 
  [(Some [("Chris", Some 19)], 5)]
;;



(*>* Problem 2c *>*)

let prob2c : (int * int -> int) * (float -> float -> unit) * bool  =
((fun (x,y) -> x+y+1), (fun x y -> ()), true)
;;


(* Fill in ??? with something that makes these typecheck: *)
(*>* Problem 2d *>*)

let prob2d =
let rec foo bar =
  match bar with
  | (a, (b, c)) :: xs -> if a then (b + c + (foo xs)) else foo xs
  | _ -> 0
in foo [(true, (5,6))]
;;


(*************)
(* PROBLEM 3 *)
(*************)

(* Consider the following terribly written function: *)

let rec zardoz f ls acc =
  if (((List.length ls) = 1) = true) then (f (List.hd(ls)) (acc))
else if (((List.length ls) = 0) = true) then acc
else let hd = List.hd(ls) in
let tl = List.tl(ls) in
let ans = f (hd) (acc) in
let ans = zardoz f tl ans in
ans
;;

(* Rewrite the code above so that it does the same thing
 * but style-wise is far superior.  
 * Write some assert statements
 * to check that your function is doing the same thing as the original.  
* Use the COS 326 style guide. *)
let rec myzardoz f ls acc = 
  match ls with
  | [] -> acc
  | hd::tl -> myzardoz f tl (f hd acc)
;;

let ls1 = [1; 2; 3];;
let add x y = x+y;; 
assert ((zardoz add ls1 0) = (myzardoz add ls1 0));;

let ls2 = [1225; 1296; 1369; 1444; 1521; 1600];;
let mult_five x y = x*y*5;;
assert ((zardoz mult_five ls2 1) = (myzardoz mult_five ls2 1));;

(*************)
(* PROBLEM 4 *)
(*************)

(***************************************)
(* Conway's Lost Cosmological Theorem! *)
(***************************************)

(* 

If l is any list of integers, the look-and-say list of s is obtained by 
reading off adjacent groups of identical elements in s. For example, the 
look-and-say list of

l = [2; 2; 2]

is

[3; 2]

because l is exactly "three twos." Similarly, the look-and-say sequence of

l = [1; 2; 2]

is

[1; 1; 2; 2]

because l is exactly "one ones, then two twos."

We will use the term run to mean a maximal length sublist of a list with 
all equal elements. For example,

[1; 1; 1] and [5]

are both runs of the list

[1; 1; 1; 5; 2]

but

[1; 1] and [5; 2] and [1; 2]

are not: 

[1; 1] is not maximal
[5; 2] has unequal elements
[1; 2] is not a sublist.

You will now define a function look and say that computes the 
look-and-say sequence of its argument.  

For full credit your solution should be a linear time solution.

CULTURAL ASIDE:

The title of this problem comes from a theorem about the sequence generated 
by repeated applications of the "look and say" operation. As look and say 
has type int list -> int list, the function can be applied to its own result. 
For example, if we start with the list of length one consisting of just the 
number 1, we get the following first 6 elements of the sequence:

[1]
[1,1]
[2,1]
[1,2,1,1]
[1,1,1,2,2,1]
[3,1,2,2,1,1]

Conway's theorem states that any element of this sequence will "decay" 
(by repeated applications of look and say) into a "compound" made up of 
combinations of "primitive elements" (there are 92 of them, plus 2 
infinite families) in 24 steps. If you are interested in this sequence, 
you may wish to consult [Conway(1987)] or other papers about the 
"look and say" operation.
*) 

let look_and_say (xs: int list) : int list = 
  (* x is the first occurrence, default is one *)
  let rec num_occurrences x ls =
    match ls with 
    | [] -> 1
    | hd::tl -> 
    if (hd = x) then (1 + num_occurrences x tl)
    else 1 
  in
  let rec las_iterative ls =
    match ls with
    | [] -> []
    | hd::tl -> 
    (num_occurrences hd tl) :: hd :: (las_iterative tl)
  in
  las_iterative xs
;;

(* Testing for the look_and_say function *)
(* let print_list ls =
  let rec print_list_elems elems =
    match elems with
    | [] -> ()
    | hd::tl -> 
      match tl with
      | [] -> ((print_int hd); (print_list_elems tl))
      | _ -> ((print_int hd); (print_string "; "); (print_list_elems tl))
  in 
  (print_string "["; print_list_elems ls; print_string "]\n")
;; *)

(* print_list (look_and_say [1; 1]);; *)

(*************)
(* PROBLEM 5 *)
(*************)

(* 5a. Write a function that flattens a list of lists in to a single
 * list with all of the elements in order. eg:
 *
 * flatten [[1;2;3]; []; [4]; [5;6]] = [1;2;3;4;5;6] 
 *
*)

let rec flatten (xss:'a list list) : 'a list =
  match xss with
  | [] -> []
  | hd::tl -> 
    match hd with
    | [] -> flatten tl
    | sub_hd::sub_tl -> sub_hd::(flatten (sub_tl::tl))
;;

(* print_list (flatten [[1;2;3]; []; [4]; [5;6]]);; *)

(* 5b. Given a matrix (list of lists), return the transpose.
 * The transpose of a matrix interchanges the rows and columns.
 * For example, transpose [[1;2;3];[4;5;6]];;
 * where [1;2;3] and [4;5;6] are the rows,
 * should return [[1;4];[2;5];[3;6]].

 * raise the exception BadMatrix if the inner lists have uneven length
 * 
*)

exception BadMatrix
let badMatrix () = raise BadMatrix

let transpose (m:int list list) : int list list =
  let rec length l = 
    match l with 
    | [] -> 0
    | hd::tl -> (1 + (length tl))
  in
  let rec check_lengths_equal len mls =
    match mls with 
    | [] -> ()
    | hd::tl -> (if (length hd <> len) then badMatrix ());
    (check_lengths_equal len tl)
  in
  let rec nth_elem_in_each n ls = 
    let rec nth_elem sub_n sub_ls =
      match sub_ls with
      | [] -> 0 (*unused*)
      | hd::tl ->
      if (sub_n=0) then hd
      else (nth_elem (sub_n-1) tl)
    in
    match ls with 
    | [] -> []
    | hd::tl -> (nth_elem n hd)::(nth_elem_in_each n tl)
  in
  let rec transpose_rec ls n elem_len =
    if (n = elem_len) then []
    else (nth_elem_in_each n ls)::(transpose_rec ls (n+1) elem_len)
  in
  let m_elem_len = 
    match m with
    | [] -> 0
    | hd::tl -> (length hd)
  in
  (check_lengths_equal m_elem_len m);
  (transpose_rec m 0 m_elem_len)
;;

(*Testing for transpose*)
(* let rec print_list_list ls =
  match ls with
  | [] -> ()
  | hd::tl -> (print_list hd); print_list_list tl
;; *)

(* print_list_list (transpose [[1;2;3];[4;5;6]]);;
print_list_list (transpose [[1;2;3];[4;5;6];[7;8;9]]);; *)


(* 5c. Return the list of all permutations of the input list. eg: 
 * perm [1;2;3] = [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]] 
*)
(* test this on small inputs!! *)
let perm (items:'a list) : 'a list list =
  let rec length l = 
    match l with 
    | [] -> 0
    | hd::tl -> (1 + (length tl))
  in
  let rec map f ls =
    match ls with 
    | [] -> []
    | hd::tl -> (f hd)::(map f tl)
  in
  let rec join_lists ls1 ls2 = 
    match ls1 with
    | [] -> ls2
    | hd::tl -> hd::(join_lists tl ls2)
  in
  let rec extract ls index : ('a option) * ('a list) = 
    match ls with
    | [] -> (None, [])
    | hd::tl -> 
      if (index=0) then (Some hd, tl)
    else 
      let (item, remaining) = (extract tl (index-1)) in
      (item, hd::remaining)
  in
  let rec perms (ls : 'a list) : 'a list list =
    let rec perm_loop ls i =
      (*extract *)
      match (extract ls i) with
      | (None, _) -> [[]] (*shouldn't happen unless items is []*)
      | (Some item, []) -> [[item]]
      | (Some item, remaining) ->
        (*recur*)
        let sub_perms = perms remaining in 
        (*append item to all in sub_perms*)
        let item_perms = (map (fun sub_perm -> item::sub_perm) sub_perms) in
        (*loop, or end loop*)
        if (i-1>=0) then
        join_lists (perm_loop ls (i-1)) item_perms
        else
        item_perms
    in
    perm_loop ls ((length ls)-1)
  in
  (perms items)
;;

(* print_list_list (perm []);; *)