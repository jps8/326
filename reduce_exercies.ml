let rec reduce f default ls =
	match ls with
	| [] -> default
	| hd::tl -> f hd (reduce f default tl)
;;

print_int (reduce (fun x y -> x + y) 0 [1;2;3]);;