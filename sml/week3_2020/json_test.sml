use "test.sml";
use "operators.sml";
use "json.sml";

val () =
    case make_silly_json 1 of
       Array [Object [("b", True), ("n", (Num _))]] => 
        assert true "make_silly_json: pass"
     | _ => 
        assert false "make_silly_json"

val () =
    assert
    $ concat_with (":", []) = ""
    $ "concat_with: [] = ''"

val () =
    assert
    $ concat_with (":", ["a", "b"]) = "a:b"
    $ "concat_with: ['a', 'b']) = 'a:b'"

val () =
    assert
    $ concat_with (":", ["a", "b", "c"]) = "a:b:c"
    $ "concat_with: ['a', 'b', 'c']) = 'a:b:c'"

val () =
    assert
    $ concat_with (":", ["a"]) = "a"
    $ "concat_with: ['a']) = 'a'"

val () =
    assert
    $ quote_string "a" = "\"a\""
    $ "quote_string: a = \"a\""

val () =
    assert
    $ real_to_string_for_json 1.0 = "1.0"
    $ "real_to_string_for_json: 1.0"

val () =
    assert
    $ real_to_string_for_json ~1.0 = "-1.0"
    $ "real_to_string_for_json: ~1.0"

val () =
    assert
    $ json_to_string (String "a") = "\"a\""
    $ "json_to_string: String a = a"


val () =
    assert
    $ json_to_string (Array [String "a", String "b"]) = "[\"a\",\"b\"]"
    $ "json_to_string: Array [String \"a\", String \"b\"]"


val () =
    assert
    $ json_to_string False = "false"
    $ "json_to_string: False"

val () =
    assert
    $ json_to_string (Num 1.2) = "1.2"
    $ "json_to_string: Num 1.2"

val () =
    assert
    $ json_to_string (Object []) = "{}"
    $ "json_to_string: Object []"

val () = print (json_to_string (Object [("b", True), ("n", (Num 0.1))]))

val () =
    assert
    $ json_to_string (Object [("b", True), ("n", (Num 0.1))]) = "{\"b\":true,\"n\":0.1}"
    $ "json_to_string: Object [(\"b\", True), (\"n\", (Num 0.1))]"

val () =
    assert
    $ assoc ("key", []) = NONE
    $ "assoc: key []"

val () =
    assert
    $ assoc ("key", [("key", "value")]) = SOME "value"
    $ "assoc: key [(key, value)]"

val () =
    assert
    $ count_occurrences ([], NotSorted) = []
    $ "count_occurrences: [] = []"

val () =
    assert
    $ count_occurrences (["a"], NotSorted) = [("a", 1)]
    $ "count_occurrences: ['a'] = [('a', 1)]"

val () =
    assert
    $ count_occurrences (["a", "b"], NotSorted) = [("a", 1), ("b", 1)]
    $ "count_occurrences: ['a', 'b'] = [('a', 1), ('b', 1)]"

val () =
    assert
    $ count_occurrences (["a", "a", "b"], NotSorted) = [("a", 2), ("b", 1)]
    $ "count_occurrences: ['a', 'a', 'b'] = [('a', 2), ('b', 1)]"

val () =
    assert
    $ count_occurrences (["b", "a", "a"], NotSorted) = [("b", 1), ("a", 2)]
    $ "count_occurrences: ['b', 'a', 'a'] = [('b', 1), ('a', 2)]"

val [] =
    count_occurrences (["a", "a", "b", "a"], NotSorted)
        handle NotSorted => 
            assert true "count_occurrences: raises on unsorted list"
            |> (fn _ => [])

val () = complete ()