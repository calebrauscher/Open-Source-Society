(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* (a) Returns NONE if the string is not in the list, else return SOME lst where lst is identical to the arguement list except the string is not in it. *)
fun all_except_option(s : string, lst : string list) =
    case lst of
	[] => NONE
      | x::xs  => case same_string(x, s) of
		      true => SOME xs
		    | false  => case all_except_option(s, xs) of
				    NONE => NONE
					 | SOME y  => SOME(x::y) 

(* (b) Returns a string list that has all the strings that are in some list in subs that also has s, but s itelf should not be in the result. *)
fun get_substitutions1(subs : string list list, s : string) =
    case subs of
	[] => []
      | x::xs => let val result = all_except_option(s, x) in
		     case result of
			 NONE => get_substitutions1(xs, s)
		       | SOME lst  => lst @ get_substitutions1(xs, s)
		 end

(* (c) Returns a string list that has all the strings that are in some list in subs that also has s, but s itself should not be in the result. *)
fun get_substitutions2(subs : string list list, s : string) =
    let fun aux(xs, acc) =
	    case xs of
		[] => acc
	      | x::xs'  => let val result = all_except_option(s, x) in
			       case result of
				   NONE => aux(xs', acc)
				 | SOME lst  => aux(xs', acc @ lst)
			   end
    in
	aux(subs, [])
    end

(* (d) Returns a list of full names (type {first:string,middle:string,last:string} list). The result is all the full names you can produce by substituting for the first name (and only the first name) *)
fun similar_names(subs : string list list, {first:string,middle:string,last:string}) =
    let val first_names = first :: get_substitutions2(subs, first);
	fun helper (f_names : string list) =
	    case f_names of
		[] => []
	      | x::xs  => {first=x, middle=middle, last=last} :: helper(xs)
    in
	helper(first_names)
    end
	

	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
