use "operators.sml";
use "list.sml";

fun first str = String.sub (str, 0)
val only_capitals = filter (first >> Char.isUpper)

val size = String.size
fun choose f a b  = if f a b then a else b
fun biggest a b = size a > size b
fun bigger_or_equal a b = size a >= size b

val longest_string1 = foldl (choose biggest) ""

val longest_string2 = foldl (choose bigger_or_equal) ""

fun longest_string_helper f =
    let
        val longest_string3 = longest_string1
        val longest_string4 = longest_string2
    in
        if f (0, 0)
        then longest_string4
        else longest_string3
    end

val longest_capitalized = only_capitals >> longest_string2

val rev_string = String.explode >> List.rev >> String.implode