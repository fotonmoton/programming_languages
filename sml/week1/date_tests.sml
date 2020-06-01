use "test.sml";
use "date.sml";


val () =
    assert
        (range 1 2 = [1, 2])
        "range: generates sequence in right order" 

val () = assert (sort (fn a => fn b => a > b)  [] = []) "sort: empty list"
val () = assert (sort (fn a => fn b => a > b)  [1] = [1]) "sort: one val"
val () = 
    assert 
        (sort (fn a => fn b => a > b)  [2, 1] = [1, 2]) 
        "sort: two elements"

val () =
    assert
        (is_older ((1999, 12, 31), (1999, 12, 31)) = false)
        "is_older: same dates evaluates to false" 

val () =
    assert
        (is_older ((2000, 11, 28), (2000, 11, 29)) = true)
        "is_older: first date older than second by day" 

val () =
    assert
        (is_older ((1999, 12, 31), (1999, 11, 31)) = false)
        "is_older: first date greater by month" 

val () =
    assert
        (is_older ((1999, 12, 31), (1999, 11, 31)) = false)
        "is_older: first date greater by month" 

val () =
    assert
        (is_older ((2000, 12, 31), (1999, 12, 31)) = false)
        "is_older: first date greater by year" 

val () =
    assert
        (number_in_month ([(2000, 12, 31)], 12) = 1)
        "number_in_month: one date with exact month" 

val () =
    let
        val dates = [(2000, 12, 31), (2000, 12, 31)]
        val month = 12
        val expect = 2
    in
        assert
            (number_in_month (dates, month) = expect)
            "number_in_month: two date with exact month"
    end

val () =
    let
        val dates = [(2000, 11, 31), (2000, 12, 31)]
        val month = 11
        val expect = 1
    in
        assert
            (number_in_month (dates, month) = expect)
            "number_in_month: two date but only one matches"
    end

val () =
    let
        val dates = []
        val month = 12
        val expect = 0
    in
        assert
            (number_in_month (dates, month) = expect)
            "number_in_month: empty list should return 0"
    end

val () =
    let
        val dates = []
        val months = []
        val expect = 0
    in
        assert
            (number_in_months (dates, months) = expect)
            "number_in_months: empty list should return 0"
    end

val () =
    let
        val dates = [(2000, 11, 31), (2000, 11, 31)]
        val months = [11]
        val expect = 2
    in
        assert
            (number_in_months (dates, months) = expect)
            "number_in_months: one month matches with two dates"
    end

val () =
    let
        val dates = [(2000, 11, 31), (2000, 12, 31),  (2000, 12, 31)]
        val months = [11, 12]
        val expect = 3
    in
        assert
            (number_in_months (dates, months) = expect)
            "number_in_months: multiple dates with same month"
    end

val () =
    let
        val dates = []
        val month = 12
        val expect = []
    in
        assert
            (dates_in_month (dates, month) = expect)
            "dates_in_month: returns empty list on empty dates"
    end

val () =
    let
        val dates = [(2000, 11, 31), (2000, 12, 30),  (2000, 12, 31)]
        val month = 12
        val expect = [(2000, 12, 30),  (2000, 12, 31)]
    in
        assert
            (dates_in_month (dates, month) = expect)
            "dates_in_month: filter non matching dates"
    end

val () =
    let
        val dates = []
        val months = []
        val expect = []
    in
        assert
            (dates_in_months (dates, months) = expect)
            "dates_in_months: empty lists"
    end

val () =
    let
        val dates = [(2000, 11, 31), (2000, 12, 30),  (2000, 12, 31)]
        val months = [11]
        val expect = [(2000, 11, 31)]
    in
        assert
            (dates_in_months (dates, months) = expect)
            "dates_in_months: match one date with one month"
    end

val () =
    let
        val dates = [(2000, 11, 31), (2000, 12, 30),  (2000, 12, 31)]
        val months = [11, 12]
        val expect = dates
    in
        assert
            (dates_in_months (dates, months) = expect)
            "dates_in_months: match three dates with 2 months"
    end

val () =
    assert
        (get_nth (["first"], 1) = "first")
        "get_nth: get first element"

val () =
    assert
        (get_nth (["first", "second", "third"], 3) = "third")
        "get_nth: get third element"

val () =
    assert
        (get_nth (["first", "second", "third"], 4) = "")
        "get_nth: not found index returns empty string :)"

val () =
    assert
        (date_to_string ((2020, 06, 01)) = "May 01, 2020")
        "date_to_string: returns correct string"

val () =
    assert
        (number_before_reaching_sum (12, []) = 0)
        "number_before_reaching_sum: empty list returns zero"

val () =
    assert
        (number_before_reaching_sum (12, [1]) = 1)
        "number_before_reaching_sum: one list element"

val () =
    assert
        (number_before_reaching_sum (12, [4, 5, 1, 3]) = 3)
        "number_before_reaching_sum: list with overflowing sum"

val () =
    assert
        (number_before_reaching_sum (12, [12, 2]) = 0)
        ("number_before_reaching_sum:" ^ 
        "first element already equals to sum but there is more")

val () =
    assert (what_month 365 = 12) "what_month: last day of a year"

val () =
    let 
        val day1 = 1
        val day2 = 2
        val expect = [1, 1] 
    in
        assert 
            (month_range (day1, day2) = expect) 
            "what_month: frist 2 days of a year"
    end

val () =
    let 
        val day1 = 31
        val day2 = 33
        val expect = [1, 2, 2] 
    in
        assert 
            (month_range (day1, day2) = expect) 
            "what_month: three days in two different months"
    end

val () =
    let 
        val dates = []
        val expect = NONE 
    in
        assert (oldest dates = expect) "oldest: retruns NONE on empty list"
    end

val () =
    let 
        val dates = [(2000, 11, 31)]
        val expect = SOME (2000, 11, 31)
    in
        assert (oldest dates = expect) "oldest: retruns SOME date"
    end

val () =
    let 
        val dates = [(2020, 11, 31), (2021, 11, 31)]
        val expect = SOME (2020, 11, 31)
    in
        assert (oldest dates = expect) "oldest: retruns oldest"
    end

val () = complete ()