(* Dan Grossman, Coursera PL, HW2 Provided Tests *)

(* These are just two tests for problem 2; you will want more.

   Naturally these tests and your tests will use bindings defined 
   in your solution, in particular the officiate function, 
   so they will not type-check if officiate is not defined.
 *)
use "hw2.sml";

fun rank_to_str(rank) =
    case rank of
        Num(n) => Int.toString(n)
      | Jack => "Jack"
      | Queen => "Queen"
      | King => "King"
      | Ace => "Ace"

fun suit_to_str(suit) =
    case suit of
          Clubs => "Clubs"
        | Diamonds => "Diamonds"
        | Hearts => "Hearts"
        | Spades => "Spades"
     
fun list_to_str(lst, elt_to_str) =
    let
        fun list_to_str_helper(lst) =
            case lst of
                  [] => ""
                | lstHd::[] => elt_to_str(lstHd)
                | lstHd::lstTl => elt_to_str(lstHd) ^ ", " ^ list_to_str_helper(lstTl)
    in
        "[" ^ list_to_str_helper(lst) ^ "]"
    end
	  
fun str_to_str(str) = str
	  
fun str_list_to_str(strLst) = list_to_str(strLst, str_to_str)
	  
(*
fun str_list_to_str(strList) =
    let
        fun list_to_str_helper(strList) =
            case strList of
                  [] => ""
                | lstHd::[] => lstHd
                | lstHd::lstTl => lstHd ^ ", " ^ list_to_str_helper(lstTl)
    in
        "[" ^ list_to_str_helper(strList) ^ "]"
    end
*)
  
fun option_str_list_to_str(lst_option) =
    case lst_option of
          NONE => "NONE"
        | SOME lst => "SOME " ^ str_list_to_str(lst)
        
fun str_list_list_to_str(lst_lst) = list_to_str(lst_lst, str_list_to_str)

(*
fun str_list_list_to_str(lst_lst) = 
    case lst_lst of
          [] => ""
        | lstHd::[] => str_list_to_str(lstHd) ^ "]"
        | lstHd::lstTl => str_list_to_str(lstHd) ^ ", " ^ str_list_list_to_str(lstTl)
*)

fun name_to_str({first, middle, last}) =
    first ^ " " ^ middle ^ " " ^ last
    
fun name_list_to_str(nameLst) = list_to_str(nameLst, name_to_str)

(*
fun name_list_to_str(nameLst) =
    let 
        fun helper(lst) =
            case nameLst of
                  [] => ""
                | lstHd::[] => name_to_str(lstHd)
                | lstHd::lstTl => name_to_str(lstHd) ^ ", " ^ helper(lstTl)
    in
        "[" ^ helper(nameLst) ^ "]"
    end
*)

fun card_to_str(aSuit, aRank) =
	rank_to_str(aRank) ^ " of " ^ suit_to_str(aSuit)
	
fun card_list_to_str(aCards) = list_to_str(aCards, card_to_str)
	
fun color_to_str(aColor) =
	case aColor of
		  Red => "Red"
		| Black => "Black"
		
fun bool_to_str(b) = 
	if b
	then "True"
	else "False"
	
fun move_to_str(aMove) =
	case aMove of
		  Draw => "Draw"
		| Discard c => "Discard " ^ card_to_str(c)
		
fun move_list_to_str(aMoves) = list_to_str(aMoves, move_to_str)
       
fun fail_string(funName, args, result, expected) =
    funName ^ " FAILED on arguments: " ^ args ^ ", result: " ^ result ^ ", expected: " ^ expected ^ "\n"
       
fun test_all_except_option(str, strLst, expectedResult) =
    let val result = all_except_option(str, strLst)
    in
        if result = expectedResult
        then []
        else [fail_string("all_except_option", str ^ ", " ^ str_list_to_str(strLst), option_str_list_to_str(result), option_str_list_to_str(expectedResult))]
    end
    
fun test_get_substitutions(lists, str, expectedResult) =
    let val result = get_substitutions1(lists, str)
    in
        if result = expectedResult
        then []
        else [fail_string("get_substitutions", str_list_list_to_str(lists) ^ ", " ^ str, str_list_to_str(result), str_list_to_str(expectedResult))]
    end
    
fun test_get_substitutions2(lists, str, expectedResult) =
    let val result = get_substitutions2(lists, str)
    in
        if result = expectedResult
        then []
        else [fail_string("get_substitutions", str_list_list_to_str(lists) ^ ", " ^ str, str_list_to_str(result), str_list_to_str(expectedResult))]
    end

fun test_similar_names(similars, fullname, expectedResult) =
    let val result = similar_names(similars, fullname)
    in
        if result = expectedResult
        then []
        else [fail_string("similar_names", str_list_list_to_str(similars) ^ ", " ^ name_to_str(fullname), name_list_to_str(result), name_list_to_str(expectedResult))]
    end
    
fun test_card_color(aCard, expectedResult) =
	let val result = card_color(aCard)
	in
		if result = expectedResult
		then []
		else [fail_string("card_color", card_to_str(aCard), color_to_str(result), color_to_str(expectedResult))]
	end
	
fun test_card_value(aCard, expectedResult) =
	let val result = card_value(aCard)
	in
		if result = expectedResult
		then []
		else [fail_string("card_value", card_to_str(aCard), Int.toString(result), Int.toString(expectedResult))]
	end
	
fun test_remove_card(aCards, aCard, e, expectedResult) =
	let val result = remove_card(aCards, aCard, e)
	in
		if result = expectedResult
		then []
		else [fail_string("remove_card", card_list_to_str(aCards) ^ ", " ^ card_to_str(aCard), card_list_to_str(result), card_list_to_str(expectedResult))]
	end
	
fun test_all_same_color(aCards, expectedResult) =
	let val result = all_same_color(aCards)
	in
		if result = expectedResult
		then []
		else [fail_string("all_same_color", card_list_to_str(aCards), bool_to_str(result), bool_to_str(expectedResult))]
	end
	
fun test_sum_cards(aCards, expectedResult) =
	let val result = sum_cards(aCards)
	in
		if result = expectedResult
		then []
		else [fail_string("sum_cards", card_list_to_str(aCards), Int.toString(result), Int.toString(expectedResult))]
	end
	
fun test_score(aCards, aGoal, expectedResult) =
	let val result = score(aCards, aGoal)
	in
		if result = expectedResult
		then []
		else [fail_string("score", card_list_to_str(aCards) ^ ", " ^ Int.toString(aGoal), Int.toString(result), Int.toString(expectedResult))]
	end
	
fun test_make_moves(aCards, aMoves, aHeld, aGoal, expectedResult) =
	let val result = make_moves(aCards, aMoves, aHeld, aGoal)
	in
		if result = expectedResult
		then []
		else [fail_string(	"make_moves",
							card_list_to_str(aCards) ^ ", " ^ move_list_to_str(aMoves) ^ ", " ^ card_list_to_str(aHeld) ^ ", " ^ Int.toString(aGoal),
							card_list_to_str(result),
							card_list_to_str(expectedResult))]
	end
	
fun test_officiate(aCards, aMoves, aGoal, expectedResult) =
	let val result = officiate(aCards, aMoves, aGoal)
	in
		if result = expectedResult
		then []
		else [fail_string(	"officiate", 
							card_list_to_str(aCards) ^ ", " ^ move_list_to_str(aMoves) ^ ", " ^ Int.toString(aGoal),
							Int.toString(result),
							Int.toString(expectedResult))]
	end
	
val c1 = (Diamonds, King)
val c2 = (Hearts,  Num 3)
val c3 = (Spades, Num 8)
val c4 = (Spades, Ace)
val c5 = (Diamonds, Num 5)
val c6 = (Clubs, Queen)	

fun print_nth(lst, n) =
	if n = 1
	then print (hd lst)
	else print_nth(tl lst, n - 1)

val testResult = test_all_except_option("one", ["one", "two", "three"], SOME ["two", "three"]) @ 
    
	test_get_substitutions([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff", ["Jeffrey","Geoff","Jeffrey"]) @
    
	test_get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff", ["Jeffrey","Geoff","Jeffrey"]) @
	
	test_similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
						{first="Fred", middle="W", last="Smith"},
						[	{first="Fred", last="Smith", middle="W"},
							{first="Fredrick", last="Smith", middle="W"},
							{first="Freddie", last="Smith", middle="W"},
							{first="F", last="Smith", middle="W"}]) @
							
	test_card_color((Diamonds, King), Red) @
	
	test_card_value((Diamonds, King), 10) @
	
	test_remove_card([c1, c2, c3, c4], c3, IllegalMove, [c1, c2, c4]) @
	test_remove_card([c1, c2, c3, c4], c1, IllegalMove, [c2, c3, c4]) @
	test_remove_card([c1, c2, c3, c4], c4, IllegalMove, [c1, c2, c3]) @
	test_remove_card([c1, c2, c3], c2, IllegalMove, [c1, c3]) @
	test_remove_card([c3, c2, c1], c2, IllegalMove, [c3, c1]) @
	
	test_all_same_color([c1, c5, c2], true) @
	
	test_sum_cards([c1, c2, c5], 18) @
	
	test_score([c1, c2, c5], 28, 5) @
	
	test_make_moves([c1, c2, c3, c4, c5], [Draw, Draw, Draw, Discard c2, Draw, Discard c1, Draw, Discard c5], [], 25, [c4, c3, c1]) @
	
	test_officiate([c1, c2, c3, c4, c5], [Draw, Draw, Draw, Discard c2, Draw, Discard c1, Draw, Discard c5], 25, 12)
	
	
(*fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end
*)