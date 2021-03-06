(* Box office analysis *)

(* Contents:
    -- the movie type
    -- the studio_gross type
    -- functions for querying and transforming lists of movies
*)

(* a movie is a tuple of (title, studio, gross in millions, year) *)
type movie = string * string * float * int;;

(* a studio_gross is a pair of (studio, gross in millions) *)
type studio_gross = string * float;;

(* call bad_argument if your function receives a bad argument *)
(* do not change this exception or function                   *)
exception Bad_arg of string
let bad_arg (s:string) = raise (Bad_arg s)

(* a useful debugging routine *)
let debug s = print_string s; flush_all()

(* *** DO NOT CHANGE DEFINITIONS ABOVE THIS LINE! *** *)

(* you may add "rec" after any of the let declarations below that you
 * wish if you find doing so useful. *)


(* find the average gross of the movies in the list                  *)
(* return 0.0 if the list is empty                                   *)
(* hint: you may need to use functions float_of_int and int_of_float *)
(* hint: if you don't know what those functions do,                  *)
(*       type them in to ocaml toplevel                              *)
(* hint: recall the difference between +. and + also 0. and 0        *)
let average (movies : movie list) : float = 
  let rec length mls = 
    match mls with 
    | [] -> 0.
    | hd::tl -> (1. +. (length tl))
  in
  let rec sum_gross mls = 
    match mls with 
    | [] -> 0.
    | (title,studio,gross,year)::tl -> (gross +. (sum_gross tl))
  in
  (sum_gross movies) /. (length movies)
;;

(* return a list containing only the movies from the given decade *)
(* call bad_arg if n is not 20, 30, ..., 90, 00, 10               *)
(* Treat 0 as 00 (this is unavoidable as 00 is not represented *)
(*   differently from 0).                                      *)
(* Note any years outside the range 1920-2019 will always be discarded *)
(* but should not raise an error condition *)
let rec decade (n:int) (ms:movie list) : movie list = 
  if (n > 90 || n mod 10 <> 0) then bad_arg "n is not a decade\n";
  match ms with
  | [] -> []
  | (title,studio,gross,year)::tl -> 
  if ((year mod 100) / 10 = n / 10) then (title,studio,gross,year)::(decade n tl)
  else (decade n tl)
;;

(* return the first n items from the list *)
(* if there are fewer than n items, return all of them *)
(* call bad_arg if n is negative *)
let rec take (n:int) (l:'a list)  : 'a list =
  if (n < 0) then bad_arg "Negative take argument";
  if (n = 0) then []
  else
  match l with
  | [] -> []
  | hd::tl -> hd::(take (n-1) tl)
;;

(* return everything but the first n items from the list *)
(* if there are fewer than n items, return the empty list *)
(* call bad_arg if n is negative *)
let rec drop (n:int) (l:'a list)  : 'a list =
    if (n < 0) then bad_arg "Negative drop argument";
    if (n = 0) then l
  else
    match l with
    | [] -> []
    | hd::tl -> (drop (n-1) tl)
;;

(* return a list [x1; x2; ...; xn] with the same elements as the input l
   and where:
     leq xn xn-1
     ...
     leq x3 x2
     leq x2 x1
     are all true
*)
(* hint: define an auxiliary function "select" *)
type 'a less = 'a -> 'a -> bool;;
let selection_sort (leq:'a less) (l:'a list) : 'a list =
  (*gets smallest value according to less_than_equal*)
  let rec select less_than_equal ls = 
    match ls with
    | [] -> (None, [])
    | hd::[] -> (Some hd, [])
    | fst::snd::tl ->
      if (less_than_equal snd fst) then 
        let (smallest, remaining) = (select less_than_equal (fst::tl)) in
        (smallest, snd::remaining)
      else 
      let (smallest, remaining) = (select less_than_equal (snd::tl)) in
        (smallest, fst::remaining)
  in
  let rec sort less_than_equal ls =
    match (select less_than_equal ls) with 
    | (None, _) -> []
    | (Some item, remaining) -> item::(sort less_than_equal remaining)
  in
  sort leq l
;;
 
(* return list of movies sorted by gross (largest gross first) *)
let sort_by_gross (movies : movie list) : movie list = 
  let leq_gross mov1 mov2 = 
    (* (title,studio,gross,year) *)
    let (_,_,gross1,_) = mov1 in
    let (_,_,gross2,_) = mov2 in
    (gross1 <= gross2)
  in
  selection_sort leq_gross movies
;;

(* return list of movies sorted by year produced (largest year first) *)
let sort_by_year (movies : movie list) : movie list = 
  let leq_year mov1 mov2 = 
    (* (title,studio,gross,year) *)
    let (_,_,_,year1) = mov1 in
    let (_,_,_,year2) = mov2 in
    (year1 <= year2)
  in
  selection_sort leq_year movies
;;

(* sort list of (studio, gross in millions) by gross in millions 
 * with the largest gross first *)
let sort_by_studio (studio_grosses : studio_gross list) : studio_gross list = 
  let leq_gross studio_gross1 studio_gross2 = 
    (* (studio,gross) *)
    let (_,gross1) = studio_gross1 in
    let (_,gross2) = studio_gross2 in
    (gross1 <= gross2)
  in
  selection_sort leq_gross studio_grosses
;;

(* given list of movies,
 * return list of pairs (studio_name, total gross revenue for that studio)  *)
let by_studio (movies:movie list) : studio_gross list =
  let rec get_studio_gross name mov_ls =
    match mov_ls with 
    | [] -> (name, 0., [])
    | (title,studio,gross,year)::tl -> 
    if (studio = name) then 
      let (_, partial_gross, remaining) = (get_studio_gross name tl) in
      (name, partial_gross +. gross, remaining)
    else
      let (_, partial_gross, remaining) = (get_studio_gross name tl) in
      (name, partial_gross, (title,studio,gross,year)::remaining)
  in
  let rec movies_to_studios ls =
    match ls with 
    | [] -> []
    | (title,studio,gross,year)::tl ->
      let (name, total_gross, remaining) = get_studio_gross studio ls in
      (studio, gross)::(movies_to_studios remaining)
  in
  movies_to_studios movies
;;

(***********)
(* Testing *)
(***********)

(* Augment the testing infrastructure below as you see fit *)

(* Test Data *)

let data1 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003)
];;

let data2 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012)
];;

let data3 : movie list = [
  ("Harry Potter and the Sorcerer's Stone","WB",317.57555,2001);
  ("Star Wars: Episode II - Attack of the Clones","Fox",310.67674,2002);
  ("Return of the Jedi", "Fox", 309.306177, 1983)
];;

let data4 : movie list = [
  ("The Lord of the Rings: The Return of the King","NL",377.85,2003);
  ("The Hunger Games","LGF",374.32,2012);
  ("The Dark Knight","WB",533.34,2008);
  ("Harry Potter and the Deathly Hallows Part 2","WB",381.01,2011)
];;

(* Assertion Testing *)

(* Uncomment the following when you are ready to test your take routine *)

assert(take 0 data4 = []);;
assert(take 1 data1 = data1);;
assert(take 2 data4 = data2);;
assert(take 5 data2 = data2);;
assert(take 2 data2 = data2);;


(* Additional Testing Infrastructure *)

let stests : (unit -> movie list) list = [
  (fun () -> sort_by_gross data1);
  (fun () -> sort_by_gross data2);
  (fun () -> sort_by_gross data3);
  (fun () -> sort_by_gross data4)
];;

let check (i:int) (tests:(unit -> 'a) list) : 'a =
  if i < List.length tests && i >= 0 then
    List.nth tests i ()
  else
    failwith ("bad test" ^ string_of_int i)
;;

check 0 stests;;
check 1 stests;;
check 2 stests;;
check 3 stests;;