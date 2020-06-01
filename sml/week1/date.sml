(* year * month * day *)
type Date = int * int * int

fun |> (x, f) = f x

infix |>

fun fold f lst acc = 
    case lst of
        [] => acc
        | head :: tail => fold f tail (f head acc)

fun reverse lst =
    fold (fn elm => fn acc => elm :: acc) lst []


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
        val reversed = reverse lst
        fun f elm acc = if predicate elm then elm :: acc else acc
    in
        fold f reversed []
    end

fun is_older ((y1, m1, d1): Date, (y2, m2, d2): Date): bool =
        let
            val same_dates = y1 = y2 andalso m1 = m2 andalso d1 = d2
            val older = y1 <= y2 andalso m1 <= m2 andalso d1 <= d2
        in
            if same_dates then false else older
        end

fun number_in_month (dates: Date list, month_to_find: int): int =
    let         
        fun count_month (_, month, _) occurences =  
            if month = month_to_find
            then occurences + 1
            else occurences 
    in
        fold count_month dates 0
    end

fun number_in_months (dates: Date list, months_to_find: int list): int =
    let
        fun count_months month acc = 
            acc + number_in_month (dates, month)
    in
        fold count_months months_to_find 0
    end

fun dates_in_month (dates: Date list, in_month: int): Date list =
    let
        fun filter_dates (_, month, _) = in_month = month
    in
        filter filter_dates dates
    end