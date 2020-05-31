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
        val dates = []
        val month = 12
        val expect = 0
    in
        assert 
            (number_in_month (dates, month) = expect)
            "number_in_month: empty list"
    end

val () = complete ()