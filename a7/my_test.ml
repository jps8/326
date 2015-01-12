open Sequence
open Printf
open System
open Future 

module ParSeq = Seq(PFuture)(struct let use_mpi = true end);;
(* 
let print_tabulate =
    let arr : int array = (ParSeq.tabulate (fun x -> x * 2) 10) in
    let rec print_list ls = (
        match ls with
        | [] -> ()
        | hd::tl -> print_int hd; print_list tl)
    in
    print_list (Array.to_list arr)
;;

print_tabulate;;
 *)
(ParSeq.tabulate (fun x -> print_int x; print_char '\n'; x * 2) 10);;