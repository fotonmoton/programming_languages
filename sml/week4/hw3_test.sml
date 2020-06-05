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


val () = complete ()