open System
open Future 
open Mpi


module type S = sig
  type 'a t
  val tabulate : (int -> 'a) -> int -> 'a t
  val seq_of_array : 'a array -> 'a t
  val array_of_seq : 'a t -> 'a array
  val iter: ('a -> unit) -> 'a t -> unit
  val length : 'a t -> int
  val empty : unit  ->'a t
  val cons : 'a -> 'a t -> 'a t
  val singleton : 'a -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val nth : 'a t -> int -> 'a 
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_reduce : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
  val flatten : 'a t t -> 'a t
  val repeat : 'a -> int -> 'a t
  val zip : ('a t * 'b t) -> ('a * 'b) t
  val split : 'a t -> int -> 'a t * 'a t
  val scan: ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
end



(*******************************************************)
(* Sequential Sequences Based on a List Representation *)
(*******************************************************)

module ListSeq : S = struct

  type 'a t = 'a list

  let length = List.length

  let empty () = []

  let cons (x:'a) (s:'a t) = x::s

  let singleton x = [x]

  let append = List.append

  let tabulate f n =
    let rec helper acc x =
      if x = n then List.rev acc
      else helper ((f x)::acc) (x+1) in
    helper [] 0

  let nth = List.nth

  let filter = List.filter

  let map = List.map

  let reduce = List.fold_left

  let map_reduce m r b s = reduce r b (map m s)

  let repeat x n =
    let rec helper x n acc =
      if n = 0 then acc else helper x (n-1) (x::acc) in
    helper x n []

  let flatten = List.flatten

  let zip (s1,s2) = List.combine s1 s2

  let split s i =
    let rec helper s i acc =
      match s,i with
        | [],_ -> failwith "split"
        | _,0 -> (List.rev acc,s)
        | h::t,_ -> helper t (i-1) (h::acc) in
    helper s i []

  let iter = List.iter

  let array_of_seq = Array.of_list

  let seq_of_array = Array.to_list

  let scan f b s = 
    let (_,xs) = List.fold_left (fun (v,ls) e -> let r = f v e in (r,r::ls)) (b,[]) s in
    List.rev xs

end


(*******************************************************)
(* Parallel Sequences                                  *)
(*******************************************************)

module type SEQ_ARGS = sig 
  val use_mpi: bool
end


module Seq (Par : Future.S) (Arg : SEQ_ARGS) : S = struct

  type 'a t = 'a array


  let num_cores = System.cpu_count ()


  let tabulate (f : int->'a) (n : int) : 'a t = 
    let rec start_children child_index =
      if child_index >= num_cores then [] else
      (*returns a sublist in the correct order *)
      let child_func () : 'a list = 
        let start_index = (child_index * n) / num_cores in
        let end_index = (((child_index+1) * n) / num_cores)-1 in (*inclusive*)
        let rec loop i =
          if i > end_index then [] else
          (f i)::(loop (i+1))
        in
        loop start_index
      in
      (Par.future child_func ())::(start_children (child_index+1))
    in
    let futures_list = start_children 0 in
    let rec append_futures ls = 
      match ls with 
      | [] -> [||]
      | hd::tl -> Array.append (Array.of_list (Par.force hd)) (append_futures tl)
    in
    append_futures futures_list


  let seq_of_array a = Array.copy a


  let array_of_seq seq = Array.copy seq


  let iter f seq = 
    let rec loop n =
      if n >= Array.length seq then () else
      f (Array.get seq n);
      loop (n+1)
    in
    loop 0


  let length seq = Array.length seq


  let empty () = [||]


  let cons elem seq = Array.append ([| elem |]) seq


  let singleton elem = [| elem |]


  let append seq1 seq2 = Array.append seq1 seq2


  let nth seq i = Array.get seq i


  let map f seq = 
    let rec start_children child_index =
      if child_index >= num_cores then [] else
      let child_func () = 
        let start_index = (child_index * n) / num_cores in
        let end_index = (((child_index+1) * n) / num_cores) in (*exclusive*)
        let arr_len = end_index - start_index in
        let child_arr = Array.sub seq start_index arr_len in
        (Array.map f child_arr)
      in
      (Par.future child_func ())::(start_children (child_index+1))
    in
    let futures_list = start_children 0 in
    let rec append_futures ls = 
      match ls with 
      | [] -> [||]
      | hd::tl -> Array.append (Par.force hd) (append_futures tl)
    in
    append_futures futures_list


  let map_reduce m r b seq = failwith "implement me"


  let reduce r b seq = 
  (*fix to only use b once*)
    let rec start_children child_index =
      if child_index >= num_cores then [] else
      let child_func () = 
        let start_index = (child_index * n) / num_cores in
        let end_index = (((child_index+1) * n) / num_cores) in (*exclusive*)
        let arr_len = end_index - start_index in
        let child_arr = Array.sub seq start_index arr_len in
        (Array.fold_left r b child_arr)
      in
      (Par.future child_func ())::(start_children (child_index+1))
    in
    let futures_list = start_children 0 in
    let rec reduce_futures ls = 
      match ls with 
      | [] -> [||]
      | hd::tl -> Array.append (Par.force hd) (append_futures tl)
    in
    append_futures futures_list


  let flatten seqseq = 
    let rec loop i = 
      if i >= Array.length seqseq then [||] else
      Array.append (Array.get seqseq i) (loop (i+1))
    in
    loop 0


  let repeat elem num = Array.make num elem


  let zip (seq1,seq2) = 
    let (ls1, ls2) = (Array.to_list seq1, Array.to_list seq2) in
    let rec zip l1 l2 = 
      match (l1, l2) with
      | ([], []) -> []
      | ([], l) -> l
      | (l, []) -> l
      | (hd1::tl1, hd2::tl2) -> (hd1, hd2)::(zip tl1 tl2)
    in
    Array.of_list (zip ls1 let)


  ls2 split seq x = 
    let second_length = (Array.length seq) - x in
    (Array.sub seq 0 x) * (Array.sub seq x second_length)


  (*******************************************************)
  (* Parallel Prefix Sum                                 *)
  (*******************************************************)

  (* Here you will implement a version of the parallel prefix scan for a sequence 
   * [a0; a1; ...], the result of scan will be [f base a0; f (f base a0) a1; ...] *)
  let scan (f: 'a -> 'a -> 'a) (base: 'a) (seq: 'a t) : 'a t =
    failwith "implement me"
        
end







