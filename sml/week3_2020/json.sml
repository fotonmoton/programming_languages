use "operators.sml";
use "list.sml";

exception NotSorted

val strcmp = String.compare

type count_occurrences = (string list * exn) -> (string * int) list
val rec count_occurrences = fn (strs, exn) => 
    let
        fun repetitons str lst reps order =
            case lst of
            [] => [(str, reps)]
            | head :: tail =>
                case strcmp (str, head) of
                EQUAL => repetitons str tail (reps + 1) order
                | new_order =>
                    if order <> EQUAL andalso new_order <> order
                    then raise exn
                    else [(str, reps)] @ repetitons head tail 1 new_order
    in
        case strs of [] => [] | head :: tail => repetitons head tail 1 EQUAL
    end