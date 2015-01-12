
open Util
open Future

module Mseq = Sequence.ListSeq
type 'a mySeq =  'a Mseq.t

type profile = {
  firstname : string;
  lastname : string;
  sex : string;
  age : int;
  lo_agepref : int;
  hi_agepref : int;
  profession : string;
  has_children : bool;
  wants_children : bool;
  leisure : string;
  drinks : bool;
  smokes : bool;
  music : string;
  orientation : string;
  build : string;
  height : string
}

let convert (p : string) : profile =
  let s = String.concat " " (Str.split (Str.regexp_string "@") p) in
  Scanf.sscanf s "%s@ %s@ %s@ %d %d %d %s@ %B %B %s@ %B %B %s@ %s@ %s@ %s"
  (fun firstname lastname sex age lo_agepref hi_agepref profession has_children
       wants_children leisure drinks smokes music orientation build height ->
   { firstname = firstname;
     lastname = lastname;
     sex = sex;
     age = age;
     lo_agepref = lo_agepref;
     hi_agepref = hi_agepref;
     profession = profession;
     has_children = has_children;
     wants_children = wants_children;
     leisure = leisure;
     drinks = drinks;
     smokes = smokes;
     music = music;
     orientation = orientation;
     build = build;
     height = height
   })

let print_profile ({
     firstname = firstname;
     lastname = lastname;
     sex = sex;
     age = age;
     lo_agepref = lo_agepref;
     hi_agepref = hi_agepref;
     profession = profession;
     has_children = has_children;
     wants_children = wants_children;
     leisure = leisure;
     drinks = drinks;
     smokes = smokes;
     music = music;
     orientation = orientation;
     build = build;
     height = height } : profile) : unit =
  Printf.printf "%s %s\n" firstname lastname;
  Printf.printf "  sex: %s  age: %d  profession: %s\n" sex age profession;
  Printf.printf "  %s  %s\n" (if drinks then "social drinker" else "nondrinker") (if smokes then "smoker" else "nonsmoker");
  Printf.printf "  %s  %s\n"
    (if has_children then "has children" else "no children")
    (if wants_children then "wants children" else "does not want children");
  Printf.printf "  prefers a %s partner between the ages of %d and %d\n"
    (if (orientation="straight" && sex="F") || (orientation = "gay/lesbian" && sex="M") then "male" else "female")
    lo_agepref hi_agepref;
  Printf.printf "  likes %s music and %s\n" music leisure


let print_matches (n : string) ((p, ps) : profile * (float * profile) list) : unit =
  print_string "------------------------------\nClient: ";
  print_profile p;
  Printf.printf "\n%s best matches:\n" n;
  List.iter (fun (bci, profile) ->
    Printf.printf "------------------------------\nCompatibility index: %f\n" bci; print_profile profile) ps;
  print_endline ""



let load_profiles (filename : string) : profile list =
  let f = open_in filename in
  let rec next accum =
    match (try Some (input_line f) with End_of_file -> None) with
    | None -> accum
    | Some line -> next ((convert line)::accum) in
  let docs = next [] in
  close_in f;
  docs

;;

let list_to_seq (ls : 'a list) :  'a mySeq = 
  Mseq.seq_of_array (Array.of_list ls)
;;

let seq_to_option (sequ: 'a mySeq) : 'a option mySeq = 
  let optioner (cur: 'a) : 'a option = Some cur in 
  Mseq.map optioner sequ
;;

module PSeq = Sequence.Seq(PFuture)(struct let use_mpi = true end)

(* apm computes the potential love of your life.  The filename of a file
 * containing candidate profiles is in location 0 of the given array.
 * The number of matches desired is in location 1.  The first and last name
 * of the (human) client are in location 2 and 3, respectively.  The client's
 * profile must be in the profiles file.  Results are output to stdout. *)
let matchme (args : string array) : unit = 
  let filename : string = Array.get args 0 in
  let num_matches : int = int_of_string (Array.get args 1) in
  let first_name : string = Array.get args 2 in
  let last_name : string = Array.get args 3 in

  let prof_seq : profile mySeq = list_to_seq (load_profiles filename) in



  let prof_finder (fname:string)(lname:string)(ps:profile mySeq) : profile = 

    let name_match (p:profile): bool =
      if (p.firstname == fname && p.lastname == lname) then true
      else false
    in 

    let prof_match (left:profile option) (right:profile option) : profile option  = 
      match (left, right) with
          (Some l, None) -> (if name_match l then Some l
                              else None)
        | (None, Some r) -> (if name_match r then Some r
                              else None)
        | (Some lb, Some rb) -> (if name_match rb then Some rb
                                 else if name_match lb then Some lb
                                 else raise (Failure "our matcher isn't working")) 
        | (None, None) -> raise (Failure "why are we getting 2 Nones in reduce?!") 
    in

    let ps_option = seq_to_option ps in 

    match Mseq.reduce prof_match None ps_option with 
        Some a -> a
      | None -> raise (Failure "profile seq doesn't have query profile")
  in 

  let query : profile = prof_finder first_name last_name prof_seq in

  let g_o (p:profile) : float = 
    if query.orientation == p.orientation then (
      if query.orientation == "straight" then (
        if query.sex != p.sex then 1.0
        else 0.0 
      )
      else if query.sex == p.sex then 1.0
      else 0.0
    )
    else 0.0
  in  

  let age_q (p:profile) : float = 
    if (p.age >= query.lo_agepref && p.age <= query.hi_agepref) then 0.4
    else 0
  in

  let age_p (p:profile) : float = 
      if (query.age >= p.lo_agepref && query.age <= p.hi_agepref) then 0.178
    else 0
  in

  let grab_bag (p:profile) : float = 
    let f1 = if p.profession == query.profession then 0.115
             else 0
    in 
    let f2 = if p.leisure == query.leisure then (f1 + 0.0863)
              else f1
    in 
    let f3 = if p.music == query.music then (f2 + 0.0498)
              else f2
    in 
    let f4 = if p.drinks == query.drinks then (f3+0.0367)
              else f3
    in
    let f5 = if p.smokes == query.smokes

  let compatability_gen (cur : profile) : float = 
    let comp_index

  ()
;;





















