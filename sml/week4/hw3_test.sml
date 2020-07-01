use "test.sml";
use "hw3.sml";

fun comp a b = a <= b

val () =
    assert
    $ sort comp [1, 2] = [1, 2]
    $ "sort: sorting"

val () =
    assert
    $ sort comp [1, 2, 1, 0, 4, 5, 2] = [0, 1, 1, 2, 2, 4, 5]
    $ "sort: sorting x2"

val () =
    assert
    $ only_capitals ["Alpha", "beta"] = ["Alpha"]
    $ "only_capitals: filter strings"

val () =
    assert
    $ longest_string1 ["alpha", "betta"] = "alpha"
    $ "longest_string1: finds longest string"


val () =
    assert
    $ longest_string1 [] = ""
    $ "longest_string1: empty syting on empty list"


val () =
    assert
    $ longest_string2 ["alpha", "betta"] = "betta"
    $ "longest_string2: on ties it returns the string closest to the end"

val () = 
    assert
    $ longest_string_helper (op >) ["alpha", "betta"] = "alpha"
    $ "longest_string_helper: a > b = longest_string1"

val () = 
    assert
    $ longest_string_helper (op >=) ["alpha", "betta"] = "betta"
    $ "longest_string_helper: a >= b = longest_string2"

val () = 
    assert
    $ longest_capitalized ["Alpha", "bettaaaa"] = "Alpha"
    $ "longest_capitalized: longest capitalized string"

val () = 
    assert
    $ rev_string "string" = "gnirts"
    $ "rev_string: reverses string"

val NONE =
    first_answer (fn elm => NONE) []
        handle NoAnswer =>
            assert true "first_answer: rises on on empty lst" 
            |> (fn _ => NONE)

val NONE =
    first_answer (fn elm => NONE) ["elm"]
        handle NoAnswer =>
            assert true "first_answer: rises on miss" 
            |> (fn _ => NONE)

val () =
    assert
    $ first_answer (fn elm => SOME elm) ["elm"] = "elm"
    $ "first_answer: returns first answer"


val () =
    assert
    $ first_answer 
        (fn elm => if elm = "second" then SOME elm else NONE) 
        ["elm", "second"] = "second"
    $ "first_answer: returns some answer"

val () =
    assert
    $ all_answers (fn elm => SOME elm) [] = SOME []
    $ "all_answers: returns SOME [] on []"

val () =
    assert
    $ all_answers (fn elm => SOME [elm]) ["a", "b"] = SOME ["a", "b"]
    $ "all_answers: returns all answers"

val () =
    assert
    $ all_answers 
        (fn elm => if elm = "a" then SOME [elm] else NONE) 
        ["a", "b"] = NONE
    $ "all_answers: NONE on at least one NONE"

val () =
    assert
    $ count_wildcards Wildcard = 1
    $ "count_wildcards: Wildcard"

val () =
    assert
    $ count_wildcards (TupleP [Wildcard, Wildcard]) = 2
    $ "count_wildcards: TupleP"

val () =
    assert
    $ count_wildcards (ConstructorP ("wild", Wildcard)) = 1
    $ "count_wildcards: ConstructorP"

val () =
    assert
    $ count_wild_and_variable_lengths (TupleP [Wildcard, Variable "var"]) = 4
    $ "count_wild_and_variable_lengths: correct length"

val () =
    let
      val input = (
          "var", 
          TupleP [
              Wildcard, 
              Variable "var", 
              ConstructorP (
                  "cons", 
                  Variable "var")])
        val expected = 2 
    in
    assert
    $ count_some_var input = expected
    $ "count_some_var: count vars"
    end

val () =
    assert
    $ check_pat (TupleP [Variable "a", Variable "b"]) = true
    $ "check_pat: true on distinct"

val () =
    assert
    $ check_pat (TupleP [Variable "a",ConstructorP ("cons", Variable "a")]) = false
    $ "check_pat: false on same"

val () =
    assert
    $ match (Unit, Wildcard) = SOME []
    $ "match: SOME [] on wildcard"

val () =
    assert
    $ match (Unit, Variable "var") = SOME [("var", Unit)]
    $ "match: match with var"

val () =
    assert
    $ match (Unit, UnitP) = SOME []
    $ "match: match with unit"

val () =
    assert
    $ match (Const 3, ConstP 3) = SOME []
    $ "match: match with const"

val () =
    assert
    $ match (Tuple [Const 3], TupleP [Variable "var"]) = SOME [("var", Const 3)]
    $ "match: match with tuple"

val () =
    assert
    $ match (Tuple [Const 3], TupleP [Variable "var"]) = SOME [("var", Const 3)]
    $ "match: match with tuple"

val () =
    let
      val arg = (Constructor ("c", Unit), ConstructorP ("c", Variable "var"))
      val expect = SOME [("var", Unit)]
    in
        assert
        $ match arg = expect
        $ "match: match with Constructor"
    end

val () =
    assert
    $ first_match Unit [ConstP 17, Variable "v"] = SOME [("v", Unit)]
    $ "first_match: finds match"

val () =
    assert
    $ first_match Unit [ConstP 17] = NONE
    $ "first_match: handles exeption"

val () = complete ()