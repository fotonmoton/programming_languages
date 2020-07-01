use "operators.sml";
use "list.sml";

datatype pattern = 
    Wildcard 
    | Variable of string 
    | UnitP 
    | ConstP of int
    | TupleP of pattern list 
    | ConstructorP of string * pattern

datatype valu = 
    Const of int 
    | Unit 
    | Tuple of valu list 
    | Constructor of string * valu

exception NoAnswer

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

fun first_answer f lst =
    case lst of
       [] => raise NoAnswer
     | head :: tail => 
        case f head of
        NONE => first_answer f tail
        | SOME answer => answer

fun all_answers 
    (f: ('a -> 'b list option)) 
    (lst: 'a list)
    : 'b list option =
    let
        fun collect lst acc =
            case lst of
            [] => SOME acc
            | head :: tail =>
                case f head of
                NONE => NONE
                | SOME lst => collect tail (acc @ lst)
    in
        collect lst []
    end

fun sum a b = a + b
(*  functions below are too similar to leave them as is.
    Find a way how to extract reapiting code
 *)
fun count_wildcards (p: pattern) : int =
    case p of
    Wildcard => 1
    | TupleP lst => foldl (count_wildcards >> sum) 0 lst
    | ConstructorP (_, pattern) => count_wildcards pattern
    | _ => 0
 
fun collect_variables (p: pattern) : string list =
    case p of
    Variable str => [str]
    | TupleP lst => foldl (collect_variables >> append) [] lst
    | ConstructorP (_, pattern) => collect_variables pattern
    | _ => []

fun count_variable_lengths (p: pattern) : int =
    p |> collect_variables |> foldl (size >> sum) 0

fun count_wild_and_variable_lengths (p: pattern) : int =
    count_wildcards p + count_variable_lengths p

fun count_some_var (var, pattern) =
    case pattern of
       Variable v => if v = var then 1 else 0
    | TupleP lst => foldl (fn p => fn acc => acc + count_some_var (var, p)) 0 lst
    | ConstructorP (_, pattern) => count_some_var (var, pattern)
    | _ => 0

fun check_pat (p: pattern) : bool =
    p 
    |> collect_variables 
    |> sort bigger_or_equal
    |> distinct

fun match (v: valu, p: pattern) : (string * valu) list option = 
    case (p, v) of
    (Wildcard, _) => SOME []
    | (Variable a, v) => SOME [(a, v)]
    | (UnitP, Unit) => SOME []
    | (ConstP c1, Const c2) => if c1 = c2 then SOME [] else NONE
    | (TupleP ps, Tuple vs) => (vs, ps) |> ListPair.zip |> all_answers match
    | (ConstructorP (s1, p), Constructor (s2, v)) =>
        if s1 = s2 then match (v, p) else NONE
    | _ => NONE
    
fun first_match 
    (v: valu) 
    (patterns: pattern list) : 
    (string * valu) list option =
    SOME $ first_answer (fn p => match (v, p)) patterns
        handle NoAnswer => NONE