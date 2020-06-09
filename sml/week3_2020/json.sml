use "operators.sml";
use "list.sml";

exception NotSorted

datatype json =
    Num of real
    | String of string
    | False
    | True
    | Null
    | Array of json list
    | Object of (string * json) list

val strcmp = String.compare

type make_silly_json = int -> json
val make_silly_json: make_silly_json = fn num =>
    let
        fun construct num json =
            case num of
            0 => json
            | _ => 
                case json of 
                Array arr => Array $ arr @ [Object [("b", True), ("n", Num $ Real.fromInt num)]]
    in
        construct num (Array [])
    end

(* not tail recursive *)
type concat_with = string * string list -> string
val rec concat_with: concat_with = fn (separator, strings) =>
    case strings of
        [] => ""
        | one :: [] => one
        | first :: second :: rest => 
            first ^ separator ^ second ^ concat_with (separator, rest)  

(* complex, two iterations but tail recursive :) *)
val rec concat_with: concat_with = fn (separator, strings) =>
    let
        fun insert elem acc = elem :: separator :: acc
        fun concat elem acc = elem ^ acc 
    in
        case strings of
        [] => ""
        | one :: [] => one
        | head :: tail => foldl insert [head] tail |> foldl concat ""
    end  

type quote_string = string -> string
val quote_string: quote_string = fn str => "\"" ^ str ^ "\""

type real_to_string_for_json = real -> string
val real_to_string_for_json: real_to_string_for_json = fn num =>
    if num < 0.0 
    then "-" ^ (Real.toString (~num)) 
    else Real.toString num

type json_to_string = json -> string
val rec json_to_string: json_to_string = fn json =>
    let
        fun concat strs = concat_with (",", strs)

        fun convert f json = json |> foldr f [] |> concat

        fun array elem all = json_to_string elem :: all

        fun object (key, value) all = 
            (quote_string key ^ ":" ^ json_to_string value) :: all
    in
        case json of
        Num num => real_to_string_for_json num
        | String str => quote_string str
        | False => "false"
        | True => "true"
        | Null => "null"
        | Array json => "[" ^ convert array json  ^ "]"
        | Object json => "{" ^ convert object json ^ "}"
    end 

type (''a, 'b) assoc = ''a * (''a * 'b) list  -> 'b option
val rec assoc = fn (needle, lst) =>
    case lst of
    [] => NONE
    | (key, value) :: tail =>
        if key = needle 
        then SOME value 
        else assoc (needle, tail)

type count_occurrences = (string list * exn) -> (string * int) list
val rec count_occurrences: count_occurrences = fn (strs, exn) => 
    let
        fun repetitons str lst reps order acc =
            case lst of
            [] => acc @ [(str, reps)] 
            | head :: tail => case strcmp (str, head) of
                EQUAL => repetitons str tail (reps + 1) order acc
                | new_order =>  if order <> EQUAL andalso new_order <> order
                                then raise exn
                                else repetitons head tail 1 new_order (acc @ [(str, reps)])
    in
        case strs of 
        [] => [] 
        | head :: tail => repetitons head tail 1 EQUAL []
    end