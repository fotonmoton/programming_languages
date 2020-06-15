use "test.sml";
use "hw3.sml";


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


val () = complete ()