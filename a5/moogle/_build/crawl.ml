open Util ;;    
open CrawlerServices ;;
open Order ;;
open Pagerank ;;


module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct 
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct 
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s = 
  let _ = Printf.printf "%s\n" s in
  flush_all();;


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* TODO: Build an index as follows:
 * 
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
  (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict = 
    if n = 0 then d else
    match LinkSet.choose frontier with
    | None -> d
    | Some (nextLink, rest) -> (
      (* print_string ((string_of_link nextLink)^"\n");
      let rec print_link_set lset =
        match LinkSet.choose lset with
        | None -> ()
        | Some (hd, tl) -> print_string ((string_of_link hd)^" ");
        print_link_set tl
      in
      let rec print_list_of_links l = 
        match l with
          | [] -> ()
          | hd::tl -> print_string ((string_of_link hd)^" ");
          print_list_of_links tl
      in *)
      match CrawlerServices.get_page nextLink with
      | None -> d
      | Some page -> (
        let newVisited = LinkSet.insert nextLink visited in
        let rec listToDiffSet l setToRemove = 
          match l with 
          | [] -> LinkSet.empty
          | hd::tl -> (
            if LinkSet.member setToRemove hd then (listToDiffSet tl setToRemove)
            else LinkSet.insert hd (listToDiffSet tl setToRemove)
          )
        in
        let unvisitedLinks = listToDiffSet page.links newVisited in
        let newFrontier = LinkSet.union unvisitedLinks rest in
        (* print_string "\npage.links: "; print_list_of_links page.links;
        print_string "\nunvisitedLinks: "; print_link_set unvisitedLinks;
        print_string "\nvisited: "; print_link_set visited;
        print_string "\nrest: "; print_link_set rest;
        print_string "\nnewVisited: "; print_link_set newVisited;
        print_string "\nnewFrontier: "; print_link_set newFrontier;
        print_string "\n\n"; *)
        let rec updatedDict oldDict (link:LinkSet.elt) wordList = 
          match wordList with
          | [] -> oldDict
          | hd::tl -> (
            let inserted =
              match WordDict.lookup oldDict hd with 
              | None -> WordDict.insert oldDict hd (LinkSet.singleton link)
              | Some assocLinks -> WordDict.insert oldDict hd (LinkSet.insert link assocLinks)
            in
            updatedDict inserted link tl
          )
        in
        let newDict = updatedDict d nextLink page.words in
        crawl (n-1) newFrontier newVisited newDict
    ))
;;

let crawler () = 
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty
;;

(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)