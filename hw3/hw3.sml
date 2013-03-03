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


(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter(fn str => Char.isUpper(String.sub(str, 0)))

val longest_string1 =
    let fun getLongest(s1, s2) =
        if String.size(s1) > String.size(s2)
        then s1
        else s2
    in
        List.foldl getLongest ""
    end
    
val longest_string2 =
    let fun get_longest(s1, s2) =
        if String.size(s1) >= String.size(s2)
        then s1
        else s2
    in
        List.foldl get_longest ""
    end

fun longest_string_helper(compare) =
    let
        fun select(s1, s2) = 
            if compare(String.size(s1), String.size(s2))
            then s1
            else s2
    in
        List.foldl select ""
    end
    
val longest_string3 =
    longest_string_helper (fn (a, b) => a > b)
    
val longest_string4 =
    longest_string_helper (fn (a, b) => a >= b)
    
val longest_capitalized = longest_string1 o only_capitals

fun rev_string(str) = String.implode(List.rev(String.explode(str)))
    
fun first_answer(f) =
    let fun first_answer_helper(lst) =
        case f(hd lst) of
            SOME x => x
          | NONE => first_answer_helper(tl lst)
    in
        first_answer_helper
    end
    
fun all_answers(f) =
    let fun all_answers_helper(lst, acc) =
        case lst of
              [] => acc
            | head::tail => case f(head) of
                  NONE => all_answers_helper(tail, acc)
                | SOME xs => all_answers_helper(tail, xs @ acc)
    in
        (fn lst => SOME (all_answers_helper(lst, [])))
    end
    
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

val count_wildcards =
    let 
        fun f1(_) = 1
        fun f2(_) = 0
    in
        g f1 f2
    end
           
val count_wild_and_variable_lengths =
    let
        fun f1(_) = 1
        fun f2(s) = String.size(s)
    in
        g f1 f2
    end
    
fun count_some_var(exampleStr, p) =
    let
        fun f1(_) = 0
        fun f2(s) =
            if exampleStr = s
            then 1
            else 0
    in
        g f1 f2 p
    end
    
fun check_pat(p) =
    let
        fun collect_strings(p) =
            case p of
                  Variable x            => [x]
                | TupleP ps             => List.foldl (fn (p, lst) => lst @ collect_strings(p)) [] ps
                | ConstructorP(_, p)    => collect_strings(p)
                | _                     => []
        fun all_unique(lst) =
            case lst of
                  [] => true
                | head::tail => if List.exists (fn str => str = head) tail
                                then false
                                else all_unique(tail)
    in
        all_unique(collect_strings(p))
    end

(*     Wildcard matches everything and produces the empty list of bindings.
 Variable s matches any value v and produces the one-element list holding (s,v).
 UnitP matches only Unit and produces the empty list of bindings.
 ConstP 17 matches only Const 17 and produces the empty list of bindings (and similarly for other
integers).
 TupleP ps matches a value of the form Tuple vs if ps and vs have the same length and for all i, the ith
element of ps matches the ith
element of vs. The list of bindings produced is all the lists from the
nested pattern matches appended together.
 ConstructorP(s1,p) matches Constructor(s2,v) if s1 and s2 are the same string (you can compare
them with =) and p matches v. The list of bindings produced is the list from the nested pattern match.
We call the strings s1 and s2 the constructor name.
 Nothing else matches.*)

fun match(v, p) = 
    case (v, p) of
          (_, Wildcard) => []
          (v, Variable s) => [s, v]
          (Unit, UnitP) => []
          (Const x, ConstP x) => []
          (Tuple vs, TupleP ps)
    
        
val l = ["one", "Two", "three", "Fours"]
val a = longest_string3(l)
val b = longest_capitalized l
val c = rev_string("abrakadabra")
val d = first_answer (fn x => if x mod 2 = 0 then SOME x else NONE) [1, 3, 4, 5, 6]
val e = all_answers (fn s => let val chars = String.explode(s) in if Char.isUpper(hd chars) then SOME chars else NONE end) l
val f = count_wildcards(Wildcard)

val t1 = Wildcard
val t2 = Variable "one"
val t3 = Variable "hello"
val t4 = ConstructorP("some p", t1)
val t5 = ConstP 42
val t6 = TupleP [t2, t4, t1, t5]
val t7 = UnitP
val t8 = TupleP [t1, t3, t6, t7, t2]

val val2 = check_pat(t8)
val val3 = count_some_var("one", t8)
val val4 = count_wildcards(t8)
val val5 = count_wild_and_variable_lengths(t8)