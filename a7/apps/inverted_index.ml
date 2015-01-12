open Future
open Util

module PSeq = Sequence.Seq(PFuture)(struct let use_mpi = true end)

(* allows us to switch between ListSeq and our Seq module with ease *)
(* currently set to ListSeq bc Seq hasn't been implemented *)
module Mseq = Sequence.ListSeq
type 'a mySeq =  'a Mseq.t 

(* an inverse binding. instead of document to word it is a word matched
 * with a single document *)
type inv = {word : string; doc : string}


(*----------------------- GENERIC HELPER FUNCTIONS --------------------------*)

(* converts a list to a sequence *)
let list_to_seq (ls : 'a list) :  'a mySeq = 
    Mseq.seq_of_array (Array.of_list ls)
;;

(* converts a document to a sequence of type inv as defined above *)
let doc_to_invSeq (doc : document) : inv mySeq = 
    let title : string = string_of_int doc.id in
    let contents : string mySeq =  
            list_to_seq (Util.split_words doc.contents) in

    let word_to_inv (cur:string) : inv = 
        {word = String.lowercase cur; doc = title}
    in

    Mseq.map word_to_inv contents
;;

(* returns true if strings a and b are equivalent *)
let match_word (a:string) (b:string) : bool =
    if String.compare a b == 0 then true
    else false 
;;

(* returns a value from list ls that when input into the filter function
 * produces true *)
let rec listSearcher (filter:'a -> bool) (ls:'a list) : 'a option = 
    match ls with 
        [] -> None
      | hd::tl -> (if filter hd then Some hd
                    else listSearcher filter tl)
;;


(*-------------------- the actual reverse index function --------------------*)
let mkindex (args : string ) : unit = 

    (* read in documents from file, and convert to a flat inv sequence *)
    let docsSeq : document mySeq = list_to_seq (load_documents args) in
    let invSeqSeq : inv mySeq mySeq = Mseq.map doc_to_invSeq docsSeq in
    let invSeq : inv mySeq = Mseq.flatten invSeqSeq in 

    (* build a hashtable to store inverse bindings *)
    let wordsTable : (string, string list) Hashtbl.t  = Hashtbl.create 1000 in

    (* helper function to be mapped across the inv sequence. takes a word and 
    title (id as string) and adds it 2 the hashtable if no binding for the word
    exists yet. if a binding does exist, it replaces it with a binding but with
    the title id concatenated to the string list of the existing title *)
    let wordListReader (title:string) (word:string) : unit = 
        let handleFirst : string list = [] in
        let found : string list =
            try Hashtbl.find wordsTable word with Not_found -> handleFirst in
        match listSearcher (match_word title) found with
            None ->  Hashtbl.replace wordsTable word (title::found);
          | Some _ -> ()
    in

    (* feeds an inv into the wordListReader function *)
    let invUnpacker (cur:inv) : inv = 
        wordListReader cur.doc cur.word; cur
    in

    (* function that turns the output datatype of another function into unit *)
    let toUnit (input:'a) : unit = () in

    (* generates hash table *)
    toUnit (Mseq.map invUnpacker invSeq); 

    (* function that takes a key value pair and turns it into a list of 
     * (key, value) tuples *)
    let hashToList (key:string) (value:string list) 
                    (keyval:(string*string list) list) :
                    (string*string list) list = (key,value)::keyval 
    in

    (* reads through all bindings in hashtable and converts them into a 
     * list. I don't think we need parallelism here because it only passes
     * through the list of words once, which is the same list that has to be
     * passed through when the results are printed out, so its the same order
     * of magnitude and not too long. *)
    let wordsList = Hashtbl.fold hashToList wordsTable [] in 

    (* prints the results *)
    print_reduce_results wordsList 
;;