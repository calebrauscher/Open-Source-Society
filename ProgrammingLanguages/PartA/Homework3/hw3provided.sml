(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1. Retuns a string list of only strings in the provided list that start with capital letters. *)

fun only_capitals(lst : string list) =
    List.filter(fn x => Char.isUpper(String.sub(x, 0))) lst

(* 2. Returns the longest string in the list. In the case of a tie return the string closest to the beginning. *)

fun longest_string1(lst : string list) =
    List.foldl(fn (s,x) => if String.size s > String.size x then s else x) "" lst

(* 3. Returns the longest string in the list. In the case of a tie return the string closest to the end. *)
fun longest_string2(lst: string list) =
    List.foldr(fn (s,x) => if String.size s > String.size x then s else x) "" lst

(* 4. Returns the longest string in the list based on the function passed to the helper function. *)
fun longest_string_helper f xs =
    List.foldl(fn (x,acc) => if f(String.size x, String.size acc) then x else acc) "" xs

fun longest_string3(lst: string list) =
    let
	val greater = fn (x,y) => x > y
    in
	longest_string_helper greater lst
    end
	
fun longest_string4(lst: string list) =
    let
	val greater_equal = fn (x,y) => x >= y
    in
	
	longest_string_helper greater_equal lst
    end

(* 5. Returns the longest string that starts with a capitalized letter or an empty string if no strings that start with a capital letter. *)
fun longest_capitalized(lst : string list) =
    let
	val longest_string = (longest_string1 o only_capitals)
    in
	longest_string(lst)
    end

(* 6. Returns a string in reverse order. *)
fun rev_string(str : string) =
    (String.implode o List.rev o String.explode) str

(* 7. Returns v if SOME v for some v, otherwise returns NoAnswer expecption. *)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs => case f(x) of
		     NONE => first_answer f xs
		   | SOME v => v

(* If the first argument returns NONE for any element, then the result for all_answers is NONE. Else the calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of  all_Answers is SOME lst where lst = lst1, lst2, ..., lstn appended together. *)
fun all_answers f lst =
    let
	fun g (f, lst, acc) = case lst of
				      [] => SOME acc
				    | x::xs => case f(x) of
						   NONE => NONE
						 | SOME v => g(f, xs, v @ acc)
    in
	g(f, lst, [])
    end
	
				   
(* 9. Use function g to:*)
(* (a) Returns how many Wildcard patterns. *)
fun count_wildcards(p : pattern) =
    g (fn v => 1) (fn v => 0) p
(* (b) Returns how many Wildcard patterns plus the sum of the string lengths of al variables in the variable patterns it contains. *)
fun count_wild_and_variable_lengths(p : pattern) =
    g (fn v => 1) (fn s => String.size s) p
(* (c) Returns the number of times the string appears as a variable in the pattern. *)
fun count_some_var(str : string, p : pattern) =
    g (fn v => 0)(fn s => if s = str then 1 else 0) p 

(* 10. Returns true if an only if all the variables appearing in the pattern are distinct from each other. *)
fun check_pat(p : pattern) =
    let
	fun variables(p : pattern) =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (v, vs) => vs @ variables(v)) [] ps
	      | ConstructorP(_,p) => variables(p)
	      | _ => []
	fun repeats(lst : string list) =
	    case lst of
		[] => true
	      | x::xs => if List.exists (fn a => a = x) xs then false else repeats(xs)
    in
	repeats(variables(p))
    end
	
(* 11. Takes a valu * pattern and returns a (String * valu) list option, namely NONE if the pattern does not match and SOME lst where lst is the list of bindings. *)
fun match(v: valu, p: pattern) =
    case (p, v) of
	(Wildcard, _) => SOME []
      | (Variable s, _) => SOME[(s,v)]
      | (UnitP, Unit) => SOME []
      | (ConstP cp, Const cv) => if cp = cv then SOME [] else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs
				 then all_answers (fn (vs',ps') => match(vs',ps')) (ListPair.zip(vs,ps))
				 else NONE
      | (ConstructorP(s1,pp), Constructor(s2,pv)) => if s1=s2 then match(pv,pp) else NONE
      | _ => NONE 

(* Takes a value and a list of patterns and returns a (string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where lst is the list of bindings for the first pattern in the list that matches. *)
fun first_match v ps  =
    (SOME(first_answer (fn p => match(v, p)) ps)) handle NoAnswer => NONE
