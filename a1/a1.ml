(*** COS 326 Problem Set 1 ***)
(*** Chris Piller ***)
(*** cpiller ***)

let undefined : unit -> 'a = fun () -> failwith "undefined" ;;

(* 1. Please define these variables with the appropriate values.
 * Be sure that these statements all type-check after editing them.
 * You can do this by hitting Ctrl+c and then Ctrl+e in Emacs, or by
 * compiling with "ocamlbuild" in the terminal emulator *)

(* 1.a. Create a string with your first name *)
let name : string = "Chris";;

(* 1.b. Modify that string to contain both your first and last names *)
let name : string = "Chris Piller";;

(* 1.c. Create a string containing your email address *)
let email : string = "cpiller@princeton.edu";;

(* 1.d. Replace (Other "...") in class_year with the appropriate item below *)
(* ie: replace (Other "...") with Sophomore or Junior for example *)
type year = Freshman | Sophomore | Junior | Senior | Other of string;;

let class_year : year = Sophomore;;

(* 1.e. Replace the .... with what you're excited about in this course *)
let exciting : string = "I'm excited about learning how functional 
languages are efficient and terse at the same time.";;

let print = Printf.printf;;

let print_survey = 
let string_year = 
(match class_year with
 | Freshman -> "2018"
 | Sophomore -> "2017"
 | Junior -> "2016"
 | Senior -> "2015"
 | Other s -> "Other: " ^ s
) in
(print "----------------------------------------\n";
 print "Name: %s\n\n" name;
 print "Email: %s\n\n" email;
 print "Year: %s\n\n" string_year; 
 print "%s\n\n" exciting;
 print "----------------------------------------\n\n";);;

(* Problem 2 - Fill in types:
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. 
 * Note that the expressions might not do anything useful -- and in fact 
 * might even display interesting problems! -- but all you should do is fill 
* in the ???s to make them type check. *)

(* Problem 2a *)

let prob2a : string  = let greet y = "Hello " ^ y in greet "World!" ;;


(* Problem 2b *)

let prob2b : float = float_of_int( int_of_float(2.2 +. 7.7)) ;;


(*>* Problem 2c *>*)

let rec prob2c (x : char) : char =
  prob2c ( if true then prob2c x else 'h')


  (*>* Problem 2d *>*)

  let rec prob2d (y:bool) (z:bool) : bool =
  prob2d (prob2d z y) (not y);;


(* Explain in a comment why each of 3a, 3b, 3c will not compile
 * and change the code in some small way so that it does. Do not
* change the top-level type associated with the expression. *)

(*>* Problem 3a *>*)
(* Because the compare function uses the < operator, both x and y must be the
same type.  I changed 4 to 4.0 so both x and y would be floats. *)

let prob3a : bool = 
  let compare x y = x < y in 
  compare 3.9 4.0 
;;


(*>* Problem 3b *>*)
(* The n-1 and x+y expressions in line 100 must have parentheses or else the
operators will be evaluated, not the results of the operators*)

let prob3b : int = 
  let fib n =
  let rec aux n y x =
  if n <= 0 then x 
else aux (n-1) (x+y) y 
in
aux n 1 0
in
fib 10


(*>* Problem 3c *>*)
(* Because sumTo is a recursive function, the keyword rec must 
be used in defining sumTo *)

let prob3c : int =
  let rec sumTo (n:int) : int =
  if n <= 0 then 0
else n + sumTo (n-1)
in
sumTo 10
;;

(*>* Problem 4 *>*)
(* 4a: Fill in the ??? with an expression that uses x and y and has
 * the right type 
 * 
 * Also: What warning message do you get if your ??? expression does
 * not use the function "square"?
 * 
 * warning: unused variable square
*)


let prob4a =
let u = 32.0 in 
let v = 28.0 in
let square w = w *. w in
let boff (x) (y) = (square x) +. (square y) in
let d = sqrt (boff u v) in
int_of_float d
;;


(* 4b: Replace each ?? with the type of the corresponding expression,
 * and write a function f that has the correct type siguanture. Explain
* in a comment a problem that remains with the function prob4b *)

(*
  prob4b has no base case. It will recur until all resources are used.
*)

let f (a:int) (b:int) : float =
  (float_of_int (a + b))
;;

let rec prob4b (x:float) (y:int) : unit =
  prob4b (f y 4) (int_of_float x)
;;


(* 4c:  Is it possible to find types for the argument and result that
 * make the function forever type check?
 *
 * Either give correct types or explain why it is impossible:
 *
 * It is impossible. Let the type of x be x_t and the result type of
 * forever be f_t.  Then the type of forever is (x_t) -> (f_t). This will
 * never type check because ((x_t) -> (f_t)) is never equal to (x_t) and so
 * the parameter of the outside forever will never have the correct type.
 *
*)

(*
let rec forever (x:??) : ?? =
  forever forever
;; 
*)

(*>* Problem 5 *>*)

exception BadDivisors of int * int;;
let bad_divisors n m = raise (BadDivisors (n,m));;

let few_divisors (n:int) (m:int) : bool = 
  let divisible_by n d = n mod d = 0 in
  let add_if_divisible n d count = 
    if divisible_by n d then (count+1) 
    else count in
  let rec num_divisors n d count =
    if d > n then count
    else num_divisors n (d+1) (add_if_divisible n d count) in
  (num_divisors n 1 0) < m
;;

(* few_divisors n m should return true if n has fewer than m divisors, 
 * (including 1 and n) and false otherwise:
few_divisors 17 3;;
- : bool = true
# few_divisors 4 3;;
- : bool = false
# few_divisors 4 4;;
- : bool = true
*) 

(* you should call the function bad_divisors n m defined above
 * if n <= 0 or m < 0
*)

(* The type signature for few_divisors is: *)
(* few_divisors : int -> int -> bool *)

(* After writing few_divisors, uncomment the following lines to test your
 * code.  (Note: your code is not necessarily completely correct just because 
 * it passes these 3 tests.)  *)


assert(few_divisors 17 3);;
assert(not (few_divisors 4 3));;
assert(few_divisors 4 4);;

(* Problem 6 - Approximating Pi *)

exception BadPi of int;;
let bad_pi (n:int) = raise (BadPi n);;

(*>* Problem 6a *>*)
(* Sinusoidal Approximation: write the function sin_pi *)
(* sin_pi : int -> float *)
(* use the following equations to define a function that returns the ith
 * approximation of pi.  

 * Call the function bad_pi if the argument i to sin_pi is less than 0.

 * approx(0) = 3
 * approx(n+1) = approx(n) + sin(approx(n))

 * Using this approximation, you will converge on many digits of pi very
 * fast.  The first few digits of pi are 3.14159 26535 89793 23846 26433.  
 * Approximation 1 accurately predicts these digits:  3.141
 * Approximation 2 accurately predicts these digits:  3.14159 26535
 * Approximation 3 accurately predicts these digits:  3.14159 26535 89793
 * 
*)

let rec sin_pi (n:int): float =
  if (n<0) then (bad_pi n)
  else if n = 0 then 3.
  else ((sin_pi (n-1)) +. (sin (sin_pi (n-1))))
;;

(*>* Problem 6b *>*)
(* Monte Carlo Approximation: write the function monte_pi
 *
 * monte_pi : int -> float
 *
 * A Monte Carlo method relies on repeated random sampling to simulate
 * some process or compute a value.  See Wikipedia:
 * http://en.wikipedia.org/wiki/Monte_Carlo_method
 * 
 * Pi can be computed using Monte Carlo simulation through a series
 * of experiments.  Here is a single experiment:
 *
 *  -- choose a pair of random floating point numbers between 0 and 1
 *  -- call the numbers x and y 
 *  -- think of (x,y) as a point on the plane in the unit square
 *  -- test whether the point falls within the unit circle by measuring
 *     the distance from the point to the origin:  x^2 + y^2 <= 1
 *
 * Now suppose you do m experiments and in n of those experiments, the
 * random point chosen falls within the upper right quarter of the unit circle.
 * Since the area of a circle is known to be pi * r^2 and the area of
 * a square is r^2 (and here we are dealing with a radius/square side
 * of length 1), the following equations hold:

  n    quarter of area of circle     1/4 * pi * r^2
 --- = -------------------------  =  -------------- = 1/4 * pi
  m        area of square                r^2

 * Use the above information to write the function monte_pi, which 
 * takes a positive number indicating the number of random points n to
 * sample and approximates pi using that number of random points.
 * Call bad_arg when a non-positive argument is given.
 *
 * To compute some random numbers, use O'Camls Random library:
 *
 * http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html
 * 
 * We initialize the library below.
 *
 * Random.float f will return a random floating point number between 0.0 and f.
 *
 * Note: this estimation method will converge far more slowly than the
 * sinusoidal method (because the sin function already captures pi, so
 * that approximation was really cheating!).  I only had the first 2 
 * digits after 5000 trials.
 * I estimated pi at 3.141628 after 1,000,000 trials.

 * =======
 * WARNING:
 * =======
 * If you make too many recursive calls in a row, you may run in
 * to a stack overflow error that looks like this:

 * Stack overflow during evaluation (looping recursion)?

 * Do not worry about that message -- just try your code on fewer trials.
 * You don't necessarily have to be able to execute 1,000,000.  For example,
 * if you code works on 100 or 1000 recursive calls, that is just fine.
 *
 * "Too many calls" may vary on different machines. Later in the semester
 * we will discuss "tail recursion" and how to fix this problem.
 *
*)

Random.init 17;; (* Don't remove this line; Your code follows *)

exception BadArg of int;;
let bad_arg (n:int) = raise (BadArg n);;

let rec monte_pi (n:int) : float = 
  if (n<=0) then bad_arg n;
  let dist (x,y) = (x*.x) +. (y*.y) in
  let in_quarter_circle (x,y) = (dist (x,y)) <= 1. in 
  let add_if_random_inside count = 
    if (in_quarter_circle (Random.float 1., Random.float 1.)) then (count+1)
    else count in
  let rec monti_rec trials_left num_inside =
    if trials_left <= 0 then (num_inside * 4)
    else 
    monti_rec (trials_left-1) (add_if_random_inside num_inside) in
  ((float_of_int (monti_rec n 0)) /. (float_of_int n))
;;

(*************)
(* Problem 7 *)
(*************)

(* Look up another technique for approximating pi on the web.
 * As a starting point, see here:  
 *
 * http://en.wikipedia.org/wiki/Approximations_of_%CF%80
 *
 * You might be able to find other interesting articles on the web too.
 * 
 * The algorithm you choose must be capable of computing many digits of
 * pi.  Algorithms that compute just 1 approximation (such as 3 or
 * 3927/1250 or any other fixed fraction) are insufficient.  Choose 
 * an algorithm that successively approximates pi in some manner.  
 * Your algorithm may not use trigonometric functions such as sin, 
 * cos, arctan, etc.
 *
*)

(* 7a:  Explain your algorithm and your sources here:

I am choosing a continued fraction representation of pi to compute from this 
image from the wikipedia article: 
http://upload.wikimedia.org/math/0/2/7/027a69678d1c2d6fb4aa2cf4b0583891.png

The fraction has a numerator of 4 and a denominator of 1 plus the next 
term in the representation.  That next term is 1^2/(3 + [next term]).
The pattern continues with the numerator of each term being the square 
of the term number, and the denominator as (2 * [term number] - 1) plus
the next term.  The series can end by setting the next term as zero
and calculating the fraction.  This algorithm is recursive in nature
and so will work well in ocaml.

*)

(* 7b:  Implement your algorithm here. *)
(*      Your algorithm should take a positive integer parameter 
 *      which increases the precision of your approximation as it 
 *      increases. You should call bad_arg when a non-positive argument is
 *      given. Explain what the parameter is used for in your 
 *      algorithm and show some tests.
 *      The signature for your function is: custom_pi : int -> float
 
 *      Again, don't worry about "stack overflow" errors for large values
 *      of the input to custom_pi.
*)

(* the parameter n is the number of levels deep the continued fraction goes.*)
let custom_pi (n:int) : float = 
    if (n<=0) then bad_arg n;
    let square_float n = (float_of_int n) *. (float_of_int n) in
    let rec custom_pi_rec count total_n = 
      if (count>total_n) then 0.
      else 
        ((float_of_int (2*count-1)) +. 
          ((square_float count)/.(custom_pi_rec (count+1) total_n))) in
    (4. /. (custom_pi_rec 1 n))
;;

(* testing function*)
let custom_pi_test (n:int) : unit = 
  print_string ((string_of_int n) ^ " levels deep gives: " 
  ^ (string_of_float (custom_pi n)) ^ "\n")
;;

(* tests *)
custom_pi_test 1;;
custom_pi_test 10;;
custom_pi_test 100;;
custom_pi_test 1000;;
custom_pi_test 100000;;
print_string "Intentional Error: \n";;
custom_pi_test (-2); (* test if the function breaks correctly *)