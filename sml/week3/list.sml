use "operators.sml";

fun cons head tail = head :: tail

fun fold f acc lst =
    case lst of
    [] => acc
    | head :: tail => fold f (f head acc) tail

fun reverse lst =
    let
        fun f elm acc = elm :: acc
    in
        fold f [] lst
    end

fun filter predicate lst =
    let
        fun f elm acc = if predicate elm then elm :: acc else acc
    in
        lst |> fold f [] |> reverse
    end

fun empty lst = lst = []

(* not efficient but works *)
fun exists elem lst =
     lst 
     |> filter (fn needle => elem = needle) 
     |> empty 
     |> not