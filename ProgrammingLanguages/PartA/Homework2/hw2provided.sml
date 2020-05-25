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

(* (b) Returns a string list that has all the strings that are in some list in substitutions that also has s, but s itelf should not be in the result. *)
fun get_substitutions1(subs : string list list, s : string) =
    case subs of
	[] => []
      | x::xs => let val result = all_except_option(s, x) in
		     case result of
			 NONE => get_substitutions1(xs, s)
		       | SOME str_list  => str_list @ get_substitutions1(xs, s)
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
