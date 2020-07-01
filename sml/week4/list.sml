use "operators.sml";

fun cons head tail = head :: tail

fun append a b = a @ b

fun foldl f acc lst =
    case lst of
    [] => acc
    | head :: tail => foldl f (f head acc) tail

fun reverse lst = foldl cons [] lst

fun sort' f lst =
    case lst of
    [] => []
    | [one] => [one]
    | first :: second :: rest =>
        if f first second
        then first :: sort' f (second :: rest)
        else second :: sort' f (first :: rest)

(* only for sorted lists *)
fun distinct lst =
    case lst of
    [] => true
    | [_] => true
    | first :: second :: rest =>
        if first = second
        then false
        else distinct (second :: rest)

(* simple recursive convergence *)
fun fix f g x = 
    if f g x = x 
    then x 
    else fix f g (f g x)

(* naive bubble sort *)
fun sort f x = fix sort' f x

fun foldr f acc lst = lst |> reverse |> foldl f acc     

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