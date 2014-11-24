(***************** Using the Lazy module ******************)
(* Here we provide an alternate implementation of streams using
   OCaml's lazy module. We recommend that you explore the
   documentation at
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lazy.html

   In this portion, you will be reimplementing various functions that
   were defined in class and several more ...
*)

(***************** Using the Num module ******************)
(* This file uses OCaml Num library for arbitrary precision arithmetic.
 * All functions that you write where you might be inclined to use the 
 * simple int type as the type of stream should instead use the num type
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Num.html
 *
 * To load this file in the OCaml toplevel, you need to load the 
 * Num library. The .ocamlinit file distributed with this code does this 
 * for you. You could, alternately, start Ocaml with nums.cma as an argument: 
 *
 *   % ocaml nums.cma
 *
 * To use ocamlbuild, see the associated Makefile.
 *
 * Some useful operators from the num library include:
 *  (+/) addition on num
 *  (-/) subtraction on nums
 *  ( */ ) multiplication on nums -- watch the spaces or you start a comment
 *  (//) division on nums
 *  ( **/ ) power on nums
 *
 * See the documentation for more.
 *)

open Num;;

open Lazy;;

(* some useful numbers *)
let zero  : num = Int 0
let one   : num = Int 1
let two   : num = Int 2
let three : num = Int 3
let four  : num = Int 4
let five  : num = Int 5

type 'a str = Cons of 'a * 'a stream
and 'a stream = 'a str Lazy.t

(* a stream of ones *)
let rec ones : num stream = 
  lazy (
    Cons (one, ones)
  )

(*>* Problem 2.1.a *>*)
(* Implement the head and tail functions *)

let head (s:'a stream) : 'a =
  match force s with
  | Cons (hd, _) -> hd
;;

let tail (s:'a stream) : 'a stream =
  match force s with
  | Cons (_, tl) -> tl
;;

(*>* Problem 2.1.b *>*)
(* Implement map *)

let rec map (f:'a -> 'b) (s:'a stream) : 'b stream =
  match (head s, tail s) with 
  | (hd, tl) -> lazy (
    Cons ((f hd), map f tl)
  )
;;

(*>* Problem 2.1.c *>*)
(* Define the infinite stream of natural numbers *)
let rec nats : num stream = 
  let rec nats_after i = 
    lazy (
      Cons (i, nats_after (i +/ one))
    )
  in
  nats_after zero
;;

(*>* Problem 2.1.d *>*)
(* Write a function nth, which returns the nth element of a
   stream. NOTE: the function nth should be zero-indexed. In other
   words, "nth 0 s" should be equivalent to "head s". *)

let rec nth (n:num) (s:'a stream) : 'a =
  let (hd, tl) = (head s, tail s) in
  if n =/ zero then hd
  else nth (n -/ one) tl
;;

(*>* Problem 2.1.e *>*)
(* Now suppose we have two num streams s1 and s2 sorted in ascending
   order. We wish to merge these into a single stream s such that s is
   sorted in ascending order AND s HAS NO DUPLICATES. Implement this
   function TO THE EXTENT POSSIBLE --- in other words, it should return
   a good answer in most cases, but you'll run in to an issue in certain
   cases.  Use the next question to document any failure modes or 
   incompletenesses or unusual aspects of your function.
*)

let merge (s1:num stream) (s2:num stream) : num stream =
  let rec merge_aux str1 str2 last_added =
    let add_if_new new_val new_s1 new_s2 = 
      match last_added with
      | None -> merge_aux new_s1 new_s2 last_added
      | Some last -> (
        if new_val <>/ last then 
          lazy ( Cons (new_val, merge_aux new_s1 new_s2 (Some new_val)) )
        else
          merge_aux new_s1 new_s2 last_added
      )
    in
    let (hd1, tl1) = (head str1, tail str1) in
    let (hd2, tl2) = (head str2, tail str2) in (
      if hd1 =/ hd2 then add_if_new hd1 tl1 tl2
      else if hd1 </ hd2 then add_if_new hd1 tl1 str2
      else add_if_new hd2 str1 tl2
    )
  in
  merge_aux s1 s2 None
;;

(*>* Problem 2.1.f *>*)
(* What kinds of inputs cause your "merge" function above to do something
   bad?  What bad thing happens in these cases?  Answer within the string. *)

let p21f = "If s1 is a stream of ones and s2 is a stream of twos, the correct 
  result would be Cons (1, 2).  However, this code will accept the first one
  and then reject every one thereafter but will never accept a two because there
  is a possibility that the next value in the ones stream is between one and two.
  This code will never terminate with a satisfactory result in this case." ;;


(*>* Problem 2.2 *>*)

(* Define a type for an infinite spreadsheet full of cells with type 'a. 
 *
 * You are free to use any representation of infinite spreadsheets you choose.
 *
 * Each cell in the spreadsheet will have coordinates (i,j).  You can think
 * of a cell with coordinates (i,j) as the cell inhabiting the ith row and
 * jth column of the spreadsheet.  You can assume that (i,j) are both
 * non-negative.  Indices in the spreadsheet start at 0.

 * Coordinates will be represented using OCaml's arbitrary precision Num
 * library, as in the rest of this file.

 * Such a spreadsheet will need to support the following operations:
 *)

type 'a spread_sheet = ('a stream) stream;;

(* you can assume all coordinates given are non-negative *)
type coordinates = num * num ;;

(* a spreadsheet containing all zeros *)
let rec zeros : num spread_sheet = 
  let rec zero_stream = 
    lazy (
      Cons (zero, zero_stream)
    )
  in
  lazy (
    Cons (zero_stream, zeros)
  )
;;

(* return the element at the (i,j) coordinate in ss *)
let get ((i,j):coordinates) (ss:'a spread_sheet) : 'a = 
  let row = nth i ss in
  let element = nth j row in
  element
;;

(* create a new spreadsheet where the (i,j) element of the spreadsheet
 * contains f i j xij  when xij was the (i,j) element of the input spreadsheet
 *)
let map_all (f:num -> num -> 'a -> 'b) (ss:'a spread_sheet) : 'b spread_sheet = 
  (* j changes, i is constant for the row *)
  let rec single_row row_i j row_s =
    match (head row_s, tail row_s) with 
    | (hd, tl) -> lazy (
      Cons ((f row_i j hd), single_row row_i (j +/ one) tl)
    )
  in
  let rec rows_map i ss_remaining = 
    match (head ss_remaining, tail ss_remaining) with 
    | (row, remaining) -> lazy (
      Cons (single_row i zero row, rows_map (i+/one) remaining)
    )
  in
  rows_map zero ss
;;

(* create an infinite multiplication table in which every cell contains the
 * product of its indices *)
let multiplication_table = 
  let f i j prev =
    i */ j
  in
  map_all f zeros
;;

(* produce a spreadsheet in which cell (i,j) contains the ith element
 * of is and the jth element of js *)
let cross_product (is:'a stream) (js:'b stream) : ('a * 'b) spread_sheet =
  let f i j prev =
    (nth i is, nth j js)
  in
  map_all f zeros
;;

