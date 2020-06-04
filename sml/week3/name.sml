use "operators.sml";
use "list.sml";

type FullName = {first: string, middle: string, last: string}

(* too complex *)
fun all_except_option (str, lst) =
    if exists str lst
    then SOME $ filter (fn elem => elem <> str) lst
    else NONE

(* more straightforward *)
fun all_except_option (str, lst) =
    let
        val filtered = filter (fn elem => elem <> str) lst
    in
        if lst = filtered then NONE else SOME filtered
    end

(* maybe it's what assigment requires us to do but I don't like it *)
(* how to get it in 8 lines?! *)
fun all_except_option (str, lst) =
    let
        fun filter (same_lst, acc) lst =
            case lst of
            [] => (same_lst, acc)
            | head :: tail =>
                if head = str
                then filter (false andalso same_lst, acc) tail
                else filter (same_lst, head :: acc) tail
    in
        case filter (true, []) lst of
        (false, lst) => SOME $ reverse lst
        | (true, _) => NONE
    end

fun get_substitutions1 (
    substitutions: string list list,
    name: string
): string list = 
    let
        fun collect lst acc =
            case all_except_option (name, lst) of
            NONE => acc
            | SOME names => acc @ names    
    in
        fold collect [] substitutions
    end

(* lol, previous function is already tail recursive 😎 *)
fun get_substitutions2 (
    substitutions: string list list,
    name: string
): string list = get_substitutions1(substitutions, name)


fun similar_names (
    substitutions: string list list,
    {first=first, middle=middle, last=last}: FullName
): FullName list =
    let
        fun full_substitution name acc = 
            {first=name, middle=middle, last=last} :: acc
    in
        get_substitutions1 (substitutions, first)
        |> fold full_substitution [] 
        |> reverse
        |> cons {first=first, middle=middle, last=last}
    end
    