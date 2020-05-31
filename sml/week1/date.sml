(* year * month * day *)
type Date = int * int * int

fun fold f lst acc = 
    case lst of
        [] => acc
        | head::tail => fold f tail (f head acc)

fun is_older (
    (y1, m1, d1): Date,
    (y2, m2, d2): Date
): bool =
        let
            val same_dates = y1 = y2 andalso m1 = m2 andalso d1 = d2
            val older = y1 <= y2 andalso m1 <= m2 andalso d1 <= d2
        in
            if same_dates then false else older
        end

fun number_in_month (
    dates: Date list,
    month_to_find: int
): int =
    let         
        fun count_month (_, month, _) occurences =  
            if month = month_to_find
            then occurences + 1
            else occurences 
    in
        fold count_month dates 0
    end

