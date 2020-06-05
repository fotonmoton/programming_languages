use "operators.sml";

fun cons head tail = head :: tail

fun foldl f acc lst =
    case lst of
    [] => acc
    | head :: tail => foldl f (f head acc) tail

fun reverse lst = foldl cons [] lst

fun foldr f acc = foldl f acc >> reverse    

fun map f = foldr (f >> cons) []

fun filter predicate lst =
    let
        fun f elm acc = if predicate elm then elm :: acc else acc
    in
        foldr f [] lst
    end


fun empty lst = lst = []

(* not efficient but works *)
fun exists elem lst =
     lst 
     |> filter (fn needle => elem = needle) 
     |> empty
     |> not