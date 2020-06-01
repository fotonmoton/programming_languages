use "test.sml";
use "date.sml";

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

val () = complete ()