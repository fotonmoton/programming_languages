use "test.sml";
use "operators.sml";
use "json.sml";

(* val () =
    assert
    $ repeatings "a" ["a", "a", "b"] (0, [], EQUAL) = (2, ["b"], LESS)
    $ "repetings 1"

val () =
    assert
    $ repeatings "c" ["a", "a", "b"] (0, [], EQUAL) = (0, ["a", "a", "b"], GREATER)
    $ "repetings 2" *)

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