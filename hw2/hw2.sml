(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings *)
fun all_except_option(str, strLst) =
    case strLst of
        [] => NONE
      | lstHd::lstTl => if same_string(lstHd, str)
                        then SOME lstTl
                        else case all_except_option(str, lstTl) of
                            NONE => NONE
                          | SOME lst => SOME (lstHd::lst)

(* function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result *)
fun get_substitutions1(lists, str) =
    case lists of
        [] => []
      | lstHd::lstTl => case all_except_option(str, lstHd) of
                            NONE => get_substitutions1(lstTl, str)
                          | SOME lst => lst @ get_substitutions1(lstTl, str)

(* function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function *)
fun get_substitutions2(lists, str) =
    let 
		fun rev(lst) =
			let fun rev_helper(lst, acc) =
				case lst of
					  [] => acc
					| head::tail => rev_helper(tail, head::acc)
			in
				rev_helper(lst, [])
			end
        fun subs_helper(lists, str, acc) =
			case lists of
				  [] => acc
				| head::tail => case all_except_option(str, head) of
					  NONE => subs_helper(tail, str, acc)
					| SOME lst => subs_helper(tail, str, lst @ acc)
    in
        subs_helper(rev(lists), str, [])
    end

(* a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names) *)
fun similar_names(similars, {first, middle, last}) =
    let
        val allSimilars = first::get_substitutions1(similars, first)
        fun similar_helper(nameLst) =
            case nameLst of
                [] => []
              | lstHd::lstTl => {first=lstHd, middle=middle, last=last}::similar_helper(lstTl)
    in
        similar_helper(allSimilars)
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

(* function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red) *)
fun card_color(aSuit, aRank) =
    case aSuit of
          Hearts => Red
        | Diamonds => Red
        | Clubs => Black
        | Spades => Black
        

(* function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10) *)
fun card_value(aSuit, aRank) =
    case aRank of
          Ace => 11
        | Num n => n
        | _ => 10
        
(* function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. *)
fun remove_card(aCards, aCard, e) =
    case aCards of
          [] => raise e
        | lstHd::lstTl =>   if lstHd = aCard
                            then lstTl
                            else lstHd::remove_card(lstTl, aCard, e)

(* function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color *)
fun all_same_color(aCards) =
    case aCards of
          [] => true
        | [_] => true
        | head::(neck::tail) => if card_color(head) = card_color(neck)
                                then all_same_color(neck::tail)
                                else false
                                
(* function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive. *)
fun sum_cards(aCards) =
    let 
		fun sum_helper(aCards, acc) =
			case aCards of
				  [] => acc
				| head::tail => sum_helper(tail, acc + card_value(head))
	in
		sum_helper(aCards, 0)
	end
        
(* function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described above *)
fun score(aCards, goal) =
    let 
        val sum = sum_cards(aCards)
        val preliminaryScore =
            if sum > goal
            then 3 * (sum - goal)
            else goal - sum
    in
        if all_same_color(aCards)
        then preliminaryScore div 2
        else preliminaryScore
    end
    
(* function officiate, which \runs a game." It takes a card list (the card-list) a move list
(what the player \does" at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:
- The game starts with the held-cards being the empty list.
- The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
- If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
- If the player draws and the card-list is empty, the game is over. Else if drawing causes the sum of
the held-cards to exceed the goal, the game is over. Else play continues with a larger held-cards
and a smaller card-list. *)
fun make_moves(aCards, aMoves, aHeldCards, aGoal) =
	case aMoves of
		  [] => aHeldCards
		| movesHd::movesTl => case movesHd of
			  Draw => 
				let 
					val drawResult = case aCards of
						  [] => aHeldCards
						| cardsHd::cardsTl =>
							if sum_cards(aHeldCards) + card_value(cardsHd) >= aGoal
							then cardsHd::aHeldCards
							else make_moves(cardsTl, movesTl, cardsHd::aHeldCards, aGoal)
				in
					drawResult
				end
					
			| Discard c => make_moves(aCards, movesTl, remove_card(aHeldCards, c, IllegalMove), aGoal)

fun officiate(aCards, aMoves, aGoal) =
    score(make_moves(aCards, aMoves, [], aGoal), aGoal)
                            