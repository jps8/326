let rec reduce (f : 'a -> 'a -> 'a) (default : 'a) (ls : 'a list) : 'a =
	match ls with
	| [] -> default
	| hd::tl -> f hd (reduce f default tl)
;;

print_int (reduce (fun x y -> x + y) 0 [1;2;3]);;