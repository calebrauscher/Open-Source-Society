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
(* (a) Returns the color of the card. Spades and clubs are black, diamonds and hearts are red *)
fun card_color(c, _) =
    case c of
	Clubs => Black
     |  Spades => Black
     | Diamonds  => Red
     |  Hearts  => Red 
				   
(* (b) Returns the cards value (numbered cards have their number as the value, aces are 11, everything else is 10) *)
fun card_value(_, rank) =
    case rank of
	Jack => 10
      | Queen  => 10
      | King  =>  10
      | Ace  => 11
      | Num value => value

(* (c) Returns a list that has all the elements of the provided list except for the provided card. If the card is in the list more than once only remove 1. If the card is not in the list raise the exception. *)
fun remove_card(cs : card list, c : card, e) =
    let fun filter_cards (cards : card list) =
	    case cards of
		[] => []
	      | x :: xs => if x=c then xs
			   else x :: filter_cards(xs)
	val filtered_cards = filter_cards(cs)
    in
	if filtered_cards = cs then raise e
	else filtered_cards
    end
	
(* (d) Returns true if all the cards in the list are the same color. *)
fun all_same_color(cards: card list) =
    case cards of
	[] => true
     |  x :: [] => true
     | head :: (neck::rest) => if card_color(head) = card_color(neck)
			    then all_same_color(neck :: rest)
			    else false

(* (e) Returns the sum of the card values. *)
fun sum_cards(cards : card list) =
    let fun aux(cards, acc) =
	    case cards of
		[] => acc
	      | head :: rest => aux(rest, card_value(head)+acc)
    in
	aux(cards, 0)
    end

(* (f) Returns the score of the hand. If the sum of the hand is greater than the goal then the score is 3 times sum-goal. If the sum of the hand is les than the goal then the scoe is goal-sum. The score is the preliminary score unless all the held-cards are the same color, in which case the score is the preliminary score divided by 2.*)
fun score(cards: card list, goal : int) =
    let val sum = sum_cards(cards)
	val preliminary_score = if sum > goal then 3 * (sum-goal) else (goal - sum)
    in
	if all_same_color(cards) then preliminary_score div 2 else preliminary_score
    end
	
(* (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
with a larger held-cards and a smaller card-list. *)
fun officiate(cards : card list, moves : move list, goal : int) =
    let fun game(hand : card list, moves : move list, remaining_cards : card list) =
	    case moves of
		[] => score(hand, goal)
	      | x::x' => case x of
			    Discard card => game(remove_card(hand, card, IllegalMove), x', remaining_cards)
			  | Draw => case remaining_cards of
					[] => score(hand, goal)
				      | y::y' => let val new_hand = y::hand in
						     if sum_cards(new_hand) > goal then score(new_hand, goal)
						     else game(new_hand, x', y')
						 end
    in
	game([], moves, cards)
    end
	
							
						  
						  
