(* 
    Pipe operator. 
    Simply to write code like "data |> fun" instead "fun data".
    Then you can "pipe" results to functions via partial application:
    
    fun square a b = a * b
    fun sum a b = a + b

    10 |> sum 10 |> square 2 // 40      
*)
fun |> (x, f) = f x

(* apply operator *)
fun $ (f, x) = f x

(* composition operator *)
fun >> (f, g) x = g(f(x))

infix |>
infix $
infix >>

(* year * month * day *)
type Date = int * int * int

(* can't use pattern matching for now *)
(* fun fold f acc lst = 
    case lst of
    [] => acc
    | head :: tail => fold f (f head acc) tail *)

fun fold f acc lst = 
    if lst = []
    then acc
    else 
        let
            val tail = tl lst
            val head = hd lst
        in
            fold f (f head acc) tail
        end

fun reverse lst =
    let
        fun f elm acc = elm :: acc 
    in
        fold f [] lst
    end


(* non tail optimized :( *)
(* fun filter predicate lst =
    case lst of
       [] => lst
     | head :: tail => 
        if predicate head
        then head :: filter predicate tail
        else filter predicate tail *)

fun filter predicate lst = 
    let
        fun f elm acc = if predicate elm then elm :: acc else acc
    in
        lst |> fold f [] |> reverse
    end

fun range from to =
    let
        fun generate from to acc =
            if from > to
            then acc
            else generate (from + 1) to (from :: acc)
    in
        generate from to [] |> reverse
    end

fun empty lst = lst = []

(* not efficient but works *)
fun exists elem lst =
    filter (fn needle => elem = needle) lst |> empty |> not 

fun uniqe lst =
    let
        fun find_uniqe elem uniqe_elems =
            if (exists elem uniqe_elems)
            then uniqe_elems
            else elem :: uniqe_elems
    in
        fold find_uniqe [] lst
    end

(* naive sort, will blow up the stack. Can't use pattern matching for now :( *)
(* also incorrect *)
(* fun sort f lst =
    case lst of
    [] => []
    | hd :: [] => [hd]
    | first :: second :: rest =>
        if f first second
        then second :: first :: sort f rest
        else first :: second :: sort f rest *)

(* also will blow up the stack *)
fun sort' f lst =
    if lst = []
        then []
        else
            if tl lst = []
            then lst
            else
                let
                    val first = hd lst
                    val second = hd $ tl lst
                    val rest = tl $ tl lst
                    val greater = f first second
                in
                    if greater
                    then second :: sort' f (first :: rest)
                    else first :: sort' f (second :: rest)
                end

(* simple recursive convergence *)
fun fix f g x = 
    if f g x = x 
    then x 
    else fix f g (f g x)

(* naive bubble sort *)
fun sort f x = fix sort' f x

fun is_older ((y1, m1, d1): Date, (y2, m2, d2): Date): bool =
        let
            val first_days = y1 * 360 + m1 * 30 + d1
            val second_days = y2 * 360 + m2 * 30 + d2
        in
            first_days < second_days
        end

fun number_in_month (dates: Date list, month_to_find: int): int =
    let         
        fun count_month (_, month, _) occurences =  
            if month = month_to_find
            then occurences + 1
            else occurences 
    in
        fold count_month 0 dates
    end

fun number_in_months (dates: Date list, months_to_find: int list): int =
    let
        fun count_months month acc = 
            acc + number_in_month (dates, month)
    in
        fold count_months 0 months_to_find
    end

fun dates_in_month (dates: Date list, in_month: int): Date list =
    let
        fun filter_dates (_, month, _) = in_month = month
    in
        filter filter_dates dates
    end

fun dates_in_months (dates: Date list, in_months: int list): Date list =
    let
        fun filter_dates month filtered = 
            filtered @ dates_in_month (dates, month)  
    in
        fold filter_dates [] in_months
    end

fun get_nth (strings: string list, nth: int): string =
    let
        fun find str (pos, found) =
            if pos = nth
            then (pos + 1, str)
            else (pos + 1, found)
    in
        fold find (1, "") strings |> #2
    end

(* this function will create list for months on every call :(  *)
fun date_to_string ((year, month, day): Date): string =
    let
        val months = [
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November", 
            "December"
        ]

        val string_month = get_nth (months, month)

        val to_str = Int.toString

    in
        string_month ^ " " ^ to_str day ^ ", " ^ to_str year
    end

fun number_before_reaching_sum (sum: int, numbers: int list): int =
    if numbers = [] orelse hd numbers >= sum
    then 0
    else 1 + number_before_reaching_sum ((sum - hd numbers), tl numbers)

fun what_month (day: int): int =
    let
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum (day, months) + 1
    end

(* correct but too complex for such problem *)
(* fun month_range (day1: int, day2: int): int list =
    let
      fun append_month day months = what_month day :: months 
    in
       range day1 day2 |> fold append_month [] |> reverse
    end
 *)

fun month_range (day1: int, day2: int): int list =
    if day1 > day2
    then []
    else what_month day1 :: month_range (day1 + 1, day2)


fun oldest (dates: Date list): Date option =
    let
        fun compare a b = not $ is_older (a, b)
    in
        if dates = []
        then NONE
        else sort compare dates |> hd |> SOME   
    end

fun number_in_months_challenge (
    dates: Date list, 
    months_to_find: int list
): int =
    number_in_months (dates, uniqe months_to_find)

fun dates_in_months_challenge (
    dates: Date list, 
    in_months: int list
): Date list =
    dates_in_months (dates, uniqe in_months)