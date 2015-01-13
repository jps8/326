
open Util
open Future
module Mseq = Sequence.Seq(PFuture)(struct let use_mpi = true end)

(* allows us to switch between ListSeq and our Seq module with ease *)
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


(* turns a filename into a profile list *)
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

module PSeq = Sequence.Seq(PFuture)(struct let use_mpi = true end)


(*----------------------- GENERIC HELPER FUNCTIONS --------------------------*)
(* converts a list to a sequence *)
let list_to_seq (ls : 'a list) :  'a mySeq = 
  Mseq.seq_of_array (Array.of_list ls)
;;

(* converts a sequence to a list *)
let seq_to_list (sequ: 'a mySeq) : 'a list = 
  Array.to_list (Mseq.array_of_seq sequ)
;;

(* converts a sequence of type a to a sequence of type a option *)
let seq_to_option (sequ: 'a mySeq) : 'a option mySeq = 
  let optioner (cur: 'a) : 'a option = Some cur in 
  Mseq.map optioner sequ
;;

(* returns true if two strings are equivalent *)
let cmpr (a:string)(b:string) : bool = 
  String.compare a b == 0
;;

(*-------------------------- THE MATCHING ALGORITHM -------------------------*)

(* apm computes the potential love of your life.  The filename of a file
 * containing candidate profiles is in location 0 of the given array.
 * The number of matches desired is in location 1.  The first and last name
 * of the (human) client are in location 2 and 3, respectively.  The client's
 * profile must be in the profiles file.  Results are output to stdout. *)
let matchme (args : string array) : unit = 

  (* read inputs in from array *)
  let filename : string = Array.get args 0 in
  let num_matches : int = int_of_string (Array.get args 1) in
  if num_matches == 0 then raise (Failure "must request > 0 matches");
  let first_name : string = Array.get args 2 in
  let last_name : string = Array.get args 3 in

  (* read profiles from file into sequence*)
  let prof_seq : profile mySeq = list_to_seq (load_profiles filename) in

  (* returns true if p has the same name as the query first and last names *)
  let name_match (p:profile): bool =
    if (cmpr p.firstname first_name) && (cmpr p.lastname last_name) 
    then true else false
  in 

  (* finds the profile of the query names in the sequence *)
  let prof_finder (ps:profile mySeq) : profile = 
    (* returns the valid profile or none from a selection of two *)
    let prof_match (left:profile option) (right:profile option) 
      : profile option  = 
      match (left, right) with
          (Some l, None) -> (if name_match l then Some l
                              else None)
        | (None, Some r) -> (if name_match r then Some r
                              else None)
        | (Some lb, Some rb) -> (if name_match rb then Some rb
                                 else if name_match lb then Some lb
                                 else None) 
        | (None, None) -> raise 
                          (Failure "why are we getting 2 Nones in reduce?!") 
    in
    let ps_option = seq_to_option ps in 
    match Mseq.reduce prof_match None ps_option with 
        Some a -> a
      | None -> raise (Failure "profile seq doesn't have query profile")
  in 

  (* find the query profile *)
  let query : profile = prof_finder prof_seq in

  (* given a profile, computes a float profile tuple where the float is the
   * compatability value of that profile with the query *)
  let compat_gen (cur : profile) : (float*profile) = 
    let grab_bag (p:profile) : float = 
      (* these are weights for different shared qualities assigned basically 
       * at random *)
      let f1 = if cmpr p.profession query.profession then 0.115 else 0.0 in 
      let f2 = if cmpr p.leisure query.leisure then (f1 +. 0.0863) else f1 in 
      let f3 = if cmpr p.music query.music then (f2 +. 0.0498) else f2 in 
      let f4 = if p.drinks == query.drinks then (f3+.0.0472) else f3 in
      let f5 = if p.smokes == query.smokes then (f4+.0.0367) else f4 in 
      let f6 = if cmpr p.build query.build then (f5+.0.066) else f5 in 
      let f7 = if cmpr p.height query.height then (f6+.0.021) else f6 in
      let f8 = if (p.age >= query.lo_agepref && p.age <= query.hi_agepref) 
          then (f7+.0.4) else f7 in
      let f9 = if (query.age >= p.lo_agepref && query.age <= p.hi_agepref) 
          then (f8+.0.178) else f8 in 
      f9
    in 

    (* a multiplier to ensure the query gets back the correct gender and 
     * orientation candidates *)
    let gender_orientation (p:profile) : float = 
      if cmpr query.orientation p.orientation then (
        if cmpr query.orientation "straight" then (
          if not (cmpr query.sex p.sex) then 1.0
          else 0.0
        )
        else if cmpr query.sex p.sex then 1.0
        else 0.0
      )
      else 0.0
    in  

    (* a multiplier to give weight to the query and the candidate's opinions
     * on children *)
    let child (p:profile) : float = 
      if ((p.has_children && query.wants_children) 
        || (not (p.has_children && query.wants_children))) then 1.0
      else if ((query.has_children && p.wants_children) 
                || (not (query.has_children && p.wants_children))) then 0.666
      else 0.33
    in  

    (* calculates the score, packs as a tuple *)
    let score = (grab_bag cur)*.(child cur)*.(gender_orientation cur) in
    (score, cur)
  in 

  (* computes compatability vectors *)
  let compat_seq : (float*profile) mySeq = Mseq.map compat_gen prof_seq in 

  (* returns true if to_find is in sequence ps *)
  let is_in (to_find:float*profile)(ps:(float*profile) mySeq) : bool = 
    (* compares a tuple of a score and a profile, returning true if they 
     * are the same *)
    let equiv (a:float*profile) (b:float*profile) : bool = 
      match (a, b) with
      ((sa, pa), (sb, pb)) -> (if (sa == sb && (cmpr pa.firstname pb.firstname) 
                              && (cmpr pa.lastname pb.lastname))
                              then true else false) in
    let equiv_tf = equiv to_find in 

    (* sees if identical profiles are already in the list found so far *)
    let prof_match (left:(float*profile) option) 
    (right:(float*profile) option) : (float*profile) option  = 
      match (left, right) with
          (Some l, None) -> (if equiv_tf l then Some l else None)
        | (None, Some r) -> (if equiv_tf r then Some r else None)
        (* doesn't actually matter that we return the correct prof *)
        | (Some lb, Some rb) -> Some lb 
        | (None, None) -> raise (Failure "invalid profile sequence") in

    let ps_option = seq_to_option ps in 
    match Mseq.reduce prof_match None ps_option with 
      Some a -> true | None -> false
  in 

  (* returns the n most compatible profiles from compat_seq that aren't
   * in sequence soFar *)
  let rec find_n_best (n:int) (soFar:(float*profile) mySeq) 
                                          : (float*profile) mySeq = 
    (* returns true if a's score is greater than b's *)
    let greater (a:float*profile) (b:float*profile) : bool = 
      match (a, b) with
      ((sa, pa), (sb, pb)) -> if sa > sb then true else false
    in

    (* returns the more compatible of 2 profiles, unless it has 
     * already been used *)
    let is_better (left:float*profile) (right:float*profile)
                                                : (float*profile) = 
      match (left,right) with
        ((_, pl),(_, pr)) -> (if name_match pl then right
                                else if name_match pr then left
                                else if is_in left soFar then right
                                else if is_in right soFar then left
                                else if greater right left then right
                                else left)
    in 

    let nth_best = Mseq.reduce is_better (0.0, query) compat_seq in 
    let new_soFar : (float*profile) mySeq = Mseq.cons nth_best soFar in 

    (* recursive call. I think it's ok we aren't using parallelism here
     * because based on the examples given and general disgression, it seems
     * as though n will likely be relatively small compared to the size
     * of the dataset, so there isn't much of a time cost. *)
    if n == 1 then new_soFar
    else find_n_best (n-1) new_soFar
  in 

  (* prints out results *)
  let matches = find_n_best num_matches (Mseq.empty ()) in 
  print_matches (string_of_int num_matches) 
                            (query, (List.rev (seq_to_list matches)))
;;






  





















