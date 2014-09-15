let rec sumTo (n:int) : int = 
	assert(n >= 0);
	match n with
	| 0 -> 0
	| _ -> n + sumTo(n - 1)
;;

let print_test (n:int) : unit = 
	let ns = string_of_int n in
	print_string ("sum to " ^ ns ^ ": " ^ (string_of_int (sumTo n)) ^ "\n")
;;

let main () = 
	print_test 0;
	print_test 2;
	print_test 5;
	print_test 837
;;

main();;