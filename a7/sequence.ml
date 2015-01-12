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
    if n < 0 then failwith "bad arg" else
    if n = 0 then [||] else
    let cores = if num_cores > n / 2 then n / 2 else num_cores in (*practical cutoff*)
    let rec start_children child_index =
      if child_index >= cores || child_index >= n then [] else
      (*returns a sublist in the correct order *)
      let child_func () : 'a list = 
        let start_index = (child_index * n) / cores in
        let end_index = (((child_index+1) * n) / cores)-1 in (*inclusive*)
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
    if seq = [||] then [||] else
    let n = Array.length seq in
    let cores = if num_cores > n / 2 then n / 2 else num_cores in (*practical cutoff*)
    let rec start_children child_index =
      if child_index >= cores || child_index >= n then [] else
      let start_index = (child_index * n) / cores in
      let end_index = (((child_index+1) * n) / cores) in (*exclusive*)
      let arr_len = end_index - start_index in
      let child_arr = Array.sub seq start_index arr_len in
      let child_func () = 
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


  let reduce r b seq = 
    if seq = [||] then b else
    let n = Array.length seq in
    let cores = if num_cores > n / 2 then n / 2 else num_cores in (*practical cutoff*)
    let rec start_children child_index =
      if child_index >= cores || child_index >= n then [] else
      let start_index = (child_index * n) / cores in
      let end_index = (((child_index+1) * n) / cores) in (*exclusive*)
      let arr_len = end_index - start_index in
      let child_arr = Array.sub seq start_index arr_len in
      let child_func () = 
        let rec child_fold i =
          if i = arr_len -1 then Array.get child_arr i else
          r (Array.get child_arr i) (child_fold (i+1))
        in
        child_fold 0
      in
      (Par.future child_func ())::(start_children (child_index+1))
    in
    let futures_list = start_children 0 in
    let rec reduce_futures ls = 
      match ls with 
      | [] -> b
      | hd::tl -> r (Par.force hd) (reduce_futures tl)
    in
    reduce_futures futures_list


  let map_reduce m r b seq = 
    let mapped_arr = map m seq in
    reduce r b mapped_arr


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
      | ([], l) -> []
      | (l, []) -> []
      | (hd1::tl1, hd2::tl2) -> (hd1, hd2)::(zip tl1 tl2)
    in
    Array.of_list (zip ls1 ls2)


  let split seq x = 
    if Array.length seq < x then failwith "split" else (
    let second_length = ((Array.length seq) - x) in
    (Array.sub seq 0 x, Array.sub seq x second_length))


  (*******************************************************)
  (* Parallel Prefix Sum                                 *)
  (*******************************************************)

  type 'a scan_message = Single of 'a | Arr of 'a array

  (* Here you will implement a version of the parallel prefix scan for a sequence 
   * [a0; a1; ...], the result of scan will be [f base a0; f (f base a0) a1; ...] *)
  let scan (f: 'a -> 'a -> 'a) (base: 'a) (seq: 'a t) : 'a t = 
    if seq = [||] then [||] else
    let n = Array.length seq in
    let cores = if num_cores > n / 2 then n / 2 else num_cores in (*practical cutoff*)
    let rec start_children child_index =
      if child_index >= cores || child_index >= n then [] else (
        let start_index = (child_index * n) / cores in
        let end_index = (((child_index+1) * n) / cores) in (*exclusive*)
        let arr_len = end_index - start_index in
        let child_arr = Array.sub seq start_index arr_len in
        let child_func (ch : ('a scan_message, 'a) Mpi.channel) () = 
          (*upward phase*)
          let rec child_scan i =
            let new_val = f child_arr.(i-1) child_arr.(i) in
            child_arr.(i) <- new_val;
            if i < arr_len - 1 then child_scan (i+1)
          in
          if arr_len > 0 && child_index = 0 then child_arr.(0) <- f base child_arr.(0);
          if arr_len > 1 then child_scan 1; (*first value is always unchanged for upward phase*)
          (* print_string "s";
          print_int child_index;
          print_string "\n";
          flush stdout; *)
          Mpi.send ch (Single(child_arr.(arr_len - 1))); (*send subtree total*)
          if (child_index <> 0) then (
            let prev_subtrees_total = Mpi.receive ch in
            (*downward phase*)
            let add_prev_total elem = f prev_subtrees_total elem in
            let result = Array.map add_prev_total child_arr in
            (* print_string "send";
            print_int child_index;
            flush stdout; *)
            Mpi.send ch (Arr(result))
          ) 
          else
            (* print_string "\nsend";
            print_int child_index;
            flush stdout; *)
            Mpi.send ch (Arr(child_arr))
        in
        (Mpi.spawn child_func ())::(start_children (child_index+1))
      )
    in
    let channels = start_children 0 in
    let rec collect_and_distribute_totals ls = 
      let rec make_sub_totals_list l =
        match l with 
        | [] -> []
        | ch::tl -> 
          match Mpi.receive ch with
          | Single total -> total::(make_sub_totals_list tl)
          | Arr _ -> failwith "Impossible"
      in
      let sub_totals = make_sub_totals_list ls in
      let (fst_sub_total,remaining_sub_totals) =
        match sub_totals with
        | [] -> failwith "Impossible"
        | hd::tl -> (hd,tl)
      in
      let rec scan_sub_totals st_ls last_total =
        match st_ls with
        | [] -> []
        | hd::tl -> 
          let new_total = f last_total hd in
          (new_total)::(scan_sub_totals tl new_total)
      in
      let complete_sub_totals = 
        fst_sub_total::(scan_sub_totals remaining_sub_totals fst_sub_total) in
      let rec distribute_totals ch_ls st_ls =
        match ch_ls with 
        | [] -> ()
        | ch::tl -> 
          match st_ls with 
          | [] -> failwith "Impossible"
          | st_hd::st_tl -> (
            Mpi.send ch st_hd;
            distribute_totals tl st_tl
          )
      in
      let ch_ls_no_first = 
        match channels with
        | [] -> failwith "Impossible"
        | hd::tl -> tl
      in
      distribute_totals ch_ls_no_first complete_sub_totals
    in
    collect_and_distribute_totals channels;
    let rec concat_results ch_remaining = 
      match ch_remaining with
      | [] -> [||]
      | hd::tl -> (
        match Mpi.receive hd with 
        | Single _ -> failwith "Impossible"
        | Arr result -> 
          Mpi.wait_die hd;
          Array.append result (concat_results tl)
      )
    in
    let final_result = concat_results channels in
    (* print_string "\n\n";
    print_int (Array.length final_result);
    print_string "\n\n"; *)
    final_result

end