let rec zip (xs : int list) (ys : int list) : (int * int) list option =
	match (xs, ys) with
	| ([], []) -> Some []
	| ([], y :: ys_tl) -> None
	| (x :: xs_tl, []) -> None
	| (x :: xs_tl, y :: ys_tl) -> 
		match zip xs_tl ys_tl with
		| None -> None
		| Some ls -> Some ((x, y) :: ls)
;;

(*Broken*)
let rec print_pairs_list (xs: (int * int) list option) : unit =
	match xs with
	| Some [] -> unit
	| Some ((x,y) :: xs_tl) -> Printf.sprintf ("(%d,%d);" x y);
		(print_pairs_list xs_tl)
;;

print_pairs_list (zip [2; 3] [4; 5]);;