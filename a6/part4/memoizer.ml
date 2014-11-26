(* We will be using OCaml's Map (aka dictionary) library.  
 * The documentation for OCaml Maps starts here:  
 * 
 * http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Map.html
 * 
 * You can see that Map.Make is a functor for creating maps.
 *
 * If you click on "S" it will lead you here:
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml-4.00/libref/Map.S.html
 *
 * and you can read about the operations created when you use the functor
 *
 *)
module type DICT = Map.S;;

(********************************)
(* A MEMOIZER THAT DOESN'T WORK *)
(********************************)

module type POORMEMOIZER =
sig
  (* the type of the memoized function's argument *)
  type key

  (* given a function, returns a poorly memoized version of that function *)
  val memo :  (key -> 'a) -> (key -> 'a)
end

module PoorMemoizer (D : DICT) : (POORMEMOIZER with type key = D.key) =
struct
  type key = D.key

  let memo (f : key -> 'a) : key -> 'a =
    let f_memoed x =
      let history = ref (D.empty) in
      try D.find x (!history) with
	  Not_found ->
	    let result = f x in
	    history := D.add x result (!history); 
	    result
    in
    f_memoed
end

(**********************************)
(* END MEMOIZER THAT DOESN'T WORK *)
(**********************************)

(**********************************)
(* START MEMOIZER THAT DOES WORK! *)
(**********************************)

module type MEMOIZER =
sig
  type key

  val memo : ((key -> 'a) -> (key -> 'a)) -> (key -> 'a)
end

(* Task 4.2:  Finish the generic memoizer *)

module Memoizer (D : DICT) : MEMOIZER with type key = D.key =
struct
  type key = D.key

  let rec memod_f f_stepwise history = 
    fun key -> (
      match get_mem_val key history with
      | None -> (
        let result = ((f_stepwise (recur f_stepwise history)) key) in (
          history := D.add key result !history;
          result
        )
      )
      | Some value -> value
    )

  and recur f_stepwise history =
    fun key -> memod_f f_stepwise history key

  and get_mem_val key history = 
    if D.mem key !history then ( Some (D.find key !history))
    else None 

  let memo f_stepwise = 
    let history = ref D.empty in
    memod_f f_stepwise history

end
