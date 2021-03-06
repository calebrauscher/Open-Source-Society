(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals ["a","B","C"] = ["B","C"]
val test1c = only_capitals ["A","b","C"] = ["A","C"]
val test1d = only_capitals ["A","B","c"] = ["A","B"]
val test1e = only_capitals ["At","Boy","Cat"] = ["At","Boy","Cat"]
val test1e = only_capitals ["at","boy","cat"] = []

val test2a = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["Abs","bc","C"] = "Abs"
val test2c = longest_string1 ["Abs","bc","Cat2"] = "Cat2"
val test2d = longest_string1 ["A","b","C"] = "A"
val test2e = longest_string1 [] = ""

val test3a = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 ["Abs","bc","C"] = "Abs"
val test3c = longest_string2 ["Abs","bc","Cat2"] = "Cat2"
val test3d = longest_string2 ["A","b","C"] = "C"
val test3e = longest_string2 [] = ""

val test4a1 = longest_string3 ["A","bc","C"] = "bc"
val test4b1 = longest_string3 ["Abs","bc","C"] = "Abs"
val test4c1 = longest_string3 ["Abs","bc","Cat2"] = "Cat2"
val test4d1 = longest_string3 ["A","b","C"] = "A"
val test4e1 = longest_string3 [] = ""

val test4a2 = longest_string4 ["A","B","C"] = "C"
val test4b2 = longest_string4 ["Abs","bc","C"] = "Abs"
val test4c2 = longest_string4 ["Abs","bc","Cat2"] = "Cat2"
val test4d2 = longest_string4 ["A","b","C"] = "C"
val test4e2 = longest_string4 [] = ""

val test5a = longest_capitalized ["A","bc","C"] = "A"
val test5b = longest_capitalized [] = ""
val test5c = longest_capitalized ["a","bc","c"] = ""
val test5d = longest_capitalized ["A","bc","Cow"] = "Cow"
val test5e = longest_capitalized ["Ac","bc","Cc"] = "Ac"
val test5f = longest_capitalized ["A","Abc","C"] = "Abc"

val test6a = rev_string "abc" = "cba"
val test6b = rev_string "" = ""
val test6c = rev_string "Racecar" = "racecaR"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a1 = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (TupleP [Wildcard, ConstP 1, Wildcard, UnitP, Wildcard]) = 3

val test9b1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths Variable("abc") = 3

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
