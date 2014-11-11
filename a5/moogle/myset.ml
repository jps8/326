(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET = 
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it 
   * and returns that element plus the new set.  
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. See TESTING EXPLANATION *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some 
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE = 
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () = 
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) = 
struct
  open Order
  type elt = C.t 
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs = 
    match xs with 
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs = 
    match xs with 
      | [] -> [x]
      | y::ys -> (match C.compare x y with 
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs = 
    match xs with 
      | [] -> []
      | x::xs1 -> (match C.compare y x with 
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys = 
    match xs, ys with 
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with 
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x = 
    match xs with 
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs = 
    match xs with 
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e 
    
  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string = 
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    ()

  let test_intersect () =
    ()

  let test_member () =
    ()

  let test_choose () =
    ()

  let test_fold () =
    ()

  let test_is_empty () =
    ()

  let test_singleton () =
    ()

  let run_tests () = 
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) = 
struct
  module D = Dict.Make(struct
      type key = C.t
      type value = unit
      let compare = C.compare
      let string_of_key = C.string_of_t
      let string_of_value = fun x -> ""
      let gen_key unit =  C.gen ()
      let gen_key_random  unit = C.gen_random ()
      let gen_key_gt x unit = C.gen_gt x ()
      let gen_key_lt x unit = C.gen_lt x ()
      let gen_key_between x y unit = C.gen_between x y ()
      let gen_value = fun x -> ()
      let gen_pair unit = (C.gen unit, ())
          end)

  type elt = D.key
  type set = D.dict
  let empty = D.empty

  (* implement the rest of the functions in the signature! *)
  let is_empty (s:set) : bool =
    if s == empty then true
    else false  
       
  let insert (aKey:elt) (theSet:set) : set =
    D.insert theSet aKey ()

  (* same as insert x empty *)         
  let singleton (aKey:elt) :  set =
    D.insert empty aKey ()
  
  let union (setA:set) (setB:set) : set = 
    let rec unionMaker (toAdd:set) (theUnion:set) = 
      match D.choose toAdd with None -> theUnion
      | Some (newKey, newVal, newDict) -> 
        unionMaker newDict (insert newKey theUnion)
    in
    unionMaker setA setB

  let intersect (setA:set) (setB:set) : set = 
    let rec sectMaker (setC:set) (setD:set) (interSet:set) : set =
      match D.choose setC with
   None -> interSet
      | Some (newKey, newVal, newDict) ->
   (if (D.member setD newKey) then
      sectMaker newDict setD (insert newKey interSet)
    else sectMaker newDict setD interSet)
    in
    sectMaker setA setB empty
        
  (* remove an element from the set -- if the                      
   * element isn't present, does nothing. *)
  let remove (aKey:elt) (aSet:set) : set = 
    D.remove aSet aKey
       
  (* returns true iff the element is in the set *)
  let member (aSet:set) (aKey:elt) : bool = 
    D.member aSet aKey

  (* chooses some member from the set, removes it
   * and returns that element plus the new set
   * If the set is empty, returns None. *)
  let choose (aSet:set) : (elt*set) option = 
    match D.choose aSet with
      None -> None
    | Some (aKey, aVal, aDict) -> Some (aKey, aDict)
               
  (* fold a function across the elements of the set
   * in some unspecified order. *)

  let fold (toApply:(elt->'a->'a)) (aVal:'a) (aSet:set) : 'a =
    
    let dictApply (aKey:D.key) (aVal:D.value) (a:'a) : 'a = 
      toApply aKey a
    in
    D.fold dictApply aVal aSet

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)
  let testSize = 1000

  (****************************************************************)
  (* helper functions for tests *)

  (* adds a list of elts (lst) to a set d *)
  let insert_list (d: set) (lst: elt list) : set = 
    List.fold_left (fun r k -> insert k r) d lst

  (* generates a random list of size elts calling upon the passed 
   * in comparable 
   *)
  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random ()) :: (generate_random_list (size - 1))

  (* returns true if list xs contains x, false if it doesn't *)
  let rec list_contains xs x = 
    match xs with 
      | [] -> false
      | y::ys -> (if x == y then true
                  else (list_contains ys x))

  (* removes extra items that occur in ls more than once *)
  let list_removeDuplicates ls =
    let rec removeDup inList outList = 
      match inList with
        [] -> outList
      | hd::tl -> (if list_contains outList hd 
                   then removeDup tl outList
                   else removeDup tl (hd::outList))
    in
    removeDup ls []

  (* returns the intersection of two lists *)
  let list_intersect l1 l2 =
    let rec intersector xs ys lsInt = 
       match xs with
        [] -> lsInt
      | hd::tl -> (if (list_contains ys hd) 
                    then (intersector tl ys (hd::lsInt))
                    else (intersector tl ys lsInt))
    in
    intersector (list_removeDuplicates l1) l2 []


  (* returns the complement of list a in list b, that is the list 
   * of all values in list a which are not in list b
   *)
  let rec list_complement (a:'a list) (b:'a list) : 'a list = 
    match a with
      hd::tl -> (if (list_contains b hd) then 
                  list_complement tl b
                else hd::(list_complement tl b))
    | [] -> []

  (* returns the symmetric difference of lists a and b *)
  let rec list_symDif (a:'a list)(b:'a list) : 'a list = 
    List.append (list_complement a b) (list_complement b a)

(****************************************************************)
(* the actual tests *)

  let test_insert () = 
    let ourList = generate_random_list testSize in
    let ourSet = insert_list empty ourList in
    List.iter (fun k -> assert(member ourSet k)) ourList ;
    ()

  let test_remove () = 
    let elts = generate_random_list testSize in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () = 
    let l1 = generate_random_list testSize in
    let l2 = generate_random_list testSize in
    let l3 = List.append l1 l2 in
    let s3 = union (insert_list empty l1) (insert_list empty l2) in 
    List.iter (fun k -> assert (member s3 k)) l3;
    ()


  let test_intersect () = 
    (* generate 2 elt lists and sets *)
    let l1 = generate_random_list testSize in
    let l2 =  generate_random_list testSize in
    let s1 = insert_list empty l1 in
    let s2 = insert_list empty l2 in

    let s_inter = intersect s1 s2 in 
    let l_inter = list_intersect l1 l2 in
    let l_symdif = list_symDif l1 l2 in

    List.iter (fun k -> assert (member s_inter k)) l_inter;
    List.iter (fun k -> assert (not (member s_inter k))) l_symdif; 
    ()

  let test_member () = 
    let l1 = generate_random_list testSize in
    let l2 =  generate_random_list testSize in
    let l_intersect = list_intersect l1 l2 in
    let l_symdif = list_symDif l1 l2 in

    let ourSet = insert_list empty l_intersect in
    List.iter (fun k -> assert(member ourSet k)) l_intersect ;
    List.iter (fun k -> assert (not (member ourSet k))) l_symdif;
    ()


  let test_choose () = 
    let l1 = generate_random_list testSize in
    let s1 = insert_list empty l1 in
    let pair1 = choose s1 in
    let elt1 = match pair1 with
                  Some (elta, seta) -> elta
                | None -> C.gen_random ()
    in
    let newSet = match pair1 with
                    Some (elta, seta) -> seta
                  | None -> empty
    in
    let rec remove_chosen (lsIn:'a list) (lsOut:'a list) : 'a list = 
      match lsIn with 
        [] -> lsOut
      | hd::tl -> (if hd == elt1 then remove_chosen tl lsOut
                    else remove_chosen tl (hd::lsOut))
    in
    let newList = remove_chosen l1 [] in
    assert (member s1 elt1);
    List.iter (fun k -> assert (member newSet k)) newList;
    assert (not (member newSet elt1));
    ()

  let test_fold () = 
    let l1 =  generate_random_list testSize in
    let l2 = list_removeDuplicates l1 in
    let funcToFold counter b = counter+1 in
    let altFtF b counter =  counter+1 in
    let listOut = List.fold_left funcToFold 0 l2 in
    let s1 =  insert_list empty l2 in
    let setOut = fold altFtF 0 s1 in
    assert (listOut==setOut);
    ()

  let test_is_empty () = 
    let elts = generate_random_list testSize in
    let s1 = insert_list empty elts in
    assert (not (is_empty s1));
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    assert (is_empty s2);
    ()

  let test_singleton () = 
    let ourKey = C.gen_random () in
    let s1 = singleton ourKey in
    let outPair =  choose s1 in
    let outKey = match outPair with
                    Some (elta, seta) -> elta
                  | None -> C.gen_random ()
    in
    let outSet = match outPair with
                    Some (elta, seta) -> seta
                  | None -> s1
    in
    assert (outKey == ourKey);
    assert (is_empty outSet);
    ()


  (* add your test functions to run_tests *)
  let run_tests () = 
    print_string "running tests";
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    () 
end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable) ;;
IntListSet.run_tests();;

(* Create a set of ints using our DictSet functor
 * 
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)

module IntDictSet = DictSet(IntComparable) ;;
IntDictSet.run_tests();;



(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) = 
  (* Change this line to use our dictionary implementation when your are 
   * finished. *)
  DictSet (C)
  (* DictSet (C) *)

