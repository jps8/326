let rec map f xs a =
	match xs with
	| [] -> a
	| hd::tl -> (map f tl ((f hd)::a))
;;