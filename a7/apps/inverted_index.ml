open Future
open Util

module PSeq = Sequence.Seq(PFuture)(struct let use_mpi = true end)

module Mseq = Sequence.ListSeq
type 'a mySeq =  'a Mseq.t 
type inv = {word : string; doc : string}

let list_to_seq (ls : 'a list) :  'a mySeq = 
	Mseq.seq_of_array (Array.of_list ls)
;;

let doc_to_invSeq (doc : document) : inv mySeq = 
	let title : string = String.lowercase doc.title in
	let contents : string mySeq =  list_to_seq (Util.split_words doc.contents) in

	let word_to_inv (cur:string) : inv = 
		{word = String.lowercase cur; doc = title}
	in

	Mseq.map word_to_inv contents
;;

let match_word (a:string) (b:string) : bool =
	if String.compare a b == 0 then true
	else false 
;;

let rec listSearcher (filter:'a -> bool) (ls:'a list) : 'a option = 
	match ls with 
		[] -> None
	  | hd::tl -> (if filter hd then Some hd
					else listSearcher filter tl)
;;

let mkindex (args : string ) : unit = 
	let docsSeq : document mySeq = list_to_seq (load_documents args) in
	let invSeqSeq : inv mySeq mySeq = Mseq.map doc_to_invSeq docsSeq in
	let invSeq : inv mySeq = Mseq.flatten invSeqSeq in 
	let wordsTable : (string, string list) Hashtbl.t  = Hashtbl.create 1000 in

	let wordListReader (title:string) (word:string) : unit = 
		let handleFirst : string list = [] in
		let found : string list =
			try Hashtbl.find wordsTable word with Not_found -> handleFirst in
		match listSearcher (match_word title) found with
			None ->  Hashtbl.replace wordsTable word (title::found);
		  | Some _ -> ()
	in


	let invUnpacker (cur:inv) : inv = 
		wordListReader cur.doc cur.word; cur
	in

	let toUnit (input:'a) : unit = () in

	(* generates hash table *)
	toUnit (Mseq.map invUnpacker invSeq); 

	let hashToList (key:string) (value:string list) (keyval:(string*string list) list) :
					(string*string list) list = (key,value)::keyval 
	in

	let wordsList = Hashtbl.fold hashToList wordsTable [] in 

	print_reduce_results wordsList 
;;