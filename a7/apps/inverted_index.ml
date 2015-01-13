open Future
open Util

(* a set to store all the documents that correspond to a particular word *)
module StringSet = Set.Make(String)
(* a map to store word keys and StringSet bindings for each word *)
module StringMap = Map.Make(String)

(* allows us to switch between ListSeq and our Seq module with ease *)
(* currently set to Seq *)
module Mseq = Sequence.Seq(PFuture)(struct let use_mpi = true end)
type 'a mySeq =  'a Mseq.t 

(*----------------------- GENERIC HELPER FUNCTIONS --------------------------*)

(* converts a list to a sequence *)
let list_to_seq (ls : 'a list) :  'a mySeq = 
    Mseq.seq_of_array (Array.of_list ls)
;;

(* merges aset and bset, a helper function for StringMap.merge *)
let set_merger (key:string)(aset:StringSet.t option)(bset:StringSet.t option)
    : StringSet.t option = 
    match (aset,bset) with
        (Some a, None) -> Some a
      | (None, Some b) -> Some b
      | (Some x, Some y) -> 
            Some (StringSet.union x y)
      | (None, None) -> None 
;;

(* merges the maps a and b, combining the sets of documents on any overlapping
   words *)
let map_merger (a:StringSet.t StringMap.t)(b:StringSet.t StringMap.t)
    : StringSet.t StringMap.t =
    StringMap.merge set_merger a b 
;; 


(* converts a document to a map of words bound to a set of the current doc *)
let doc_to_SetMap (doc : document) : StringSet.t StringMap.t  = 
    let title : string = string_of_int doc.id in
    let contents : string mySeq =  
            list_to_seq (Util.split_words doc.contents) in

    let word_to_setmap (cur:string) : StringSet.t StringMap.t = 
        let docSet = StringSet.singleton title in
        StringMap.singleton cur docSet
    in

    let seqOfSetMaps : StringSet.t StringMap.t mySeq = 
        Mseq.map word_to_setmap contents 
    in

    Mseq.reduce map_merger StringMap.empty seqOfSetMaps
;;


(*-------------------- the actual reverse index function --------------------*)
let mkindex (args : string ) : unit = 

    (* read in documents from file, and convert to a flat inv sequence *)
    let docsSeq : document mySeq = list_to_seq (load_documents args) in

    (* turn documents into Maps of words and document sets *)
    let setMapSeq : StringSet.t StringMap.t mySeq = 
        Mseq.map doc_to_SetMap docsSeq 
    in

    (* turns sequence of Maps into one big map *)
    let setMap : StringSet.t StringMap.t = 
        Mseq.reduce map_merger StringMap.empty setMapSeq 
    in 

    (* turns Map of words bound to sets to map of words bound to lists of docs
     *)
    let docsListMap : string list StringMap.t = 
        StringMap.map StringSet.elements setMap
    in 

    (* turns Map of keys and bindgins to a (key*binding) list *)
    let wordBindingList = 
        StringMap.bindings docsListMap
    in

    (* prints the results *)
    print_reduce_results wordBindingList 
;;