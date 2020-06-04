use "test.sml";
use "card.sml";

val () =
    assert (card_color (Spade, King) = Black)
    "card_color: returns right color"

val () =
    assert (card_color (Heart, King) = Red)
    "card_color: returns right color x2"

val () =
    assert (card_value (Heart, Num 5) = 5)
    "card_value: number card"

val () =
    assert (card_value (Heart, Ace) = 11)
    "card_value: ace is 11"

val () =
    assert (card_value (Heart, Jack) = 10)
    "card_value: jack is 10"

val () =
    assert (remove_card ([(Heart, Jack)], (Heart, Jack), IllegalMove) = [])
    "remove_card: removes card"

val () =
    let
        val deck = [(Heart, Jack), (Heart, Jack)]
        val card = (Heart, Jack)
        val expected = [(Heart, Jack)]
        val exp = IllegalMove
    in
        assert 
            (remove_card (deck, card, exp) = expected)
            "remove_card: leaves duplicates"
    end

val _ =
    let
        val deck = []
        val card = (Heart, Jack)
        val expected = IllegalMove
        val exp = IllegalMove
    in
        remove_card (deck, card, exp)
            handle IllegalMove =>
                let
                    val () = assert true "remove_card: raises exception"
                in
                    []
                end
    end

val () =
    let
        val deck = []
        val expected = true
    in
        assert 
        $ all_same_color deck = expected 
        $ "all_same_color: true on empty list"
    end

val () =
    let
        val deck = [(Club, Jack)]
        val expected = true
    in
        assert 
        $ all_same_color deck = expected 
        $ "all_same_color: true on one value"
    end

val () =
    let
        val deck = [(Club, Jack), (Diamond, Jack)]
        val expected = false
    in
        assert 
        $ all_same_color deck = expected 
        $ "all_same_color: false on two different colors"
    end

val () =
    let
        val deck = []
        val expected = 0
    in
        assert 
        $ sum_cards deck = expected 
        $ "sum_cards: zero on empty list"
    end

val () =
    let
        val deck = [(Club, Jack), (Diamond, Ace)]
        val expected = 21
    in
        assert 
        $ sum_cards deck = expected 
        $ "sum_cards: correct sum"
    end

val () =
    let
        val deck = []
        val goal = 20
        val expected = 10
    in
        assert 
        $ score (deck, goal) = expected 
        $ "score: empty list returns goal / 2 because empty deck = same color"
    end

val () =
    let
        val deck = [(Club, Jack), (Diamond, Jack)]
        val goal = 21
        val expected = 1
    in
        assert 
        $ score (deck, goal) = expected 
        $ "score: when sum < goal then score = goal - sum"
    end

val () =
    let
        val deck = [(Club, Jack), (Diamond, Jack)]
        val goal = 9
        val expected = 33
    in
        assert 
        $ score (deck, goal) = expected 
        $ "score: when sum > goal then score = 3 * (sum - goal)"
    end

val () =
    let
        val deck = [(Club, Jack), (Club, Ace)]
        val goal = 18
        val expected = 4
    in
        assert 
        $ score (deck, goal) = expected 
        $ "score: when all cards are the same color score is divied by 2"
    end

val () =
    let
        val deck = [(Club, Jack), (Club, Ace)]
        val moves = [Draw, Draw]
        val goal = 18
        val expected = 4
    in
        assert 
        $ officiate (deck, moves, goal) = expected 
        $ "officiate: player draw all deck, get correct result"
    end
    

val () =
    let
        val deck = [(Club, Jack)]
        val moves = [Draw, Draw]
        val goal = 21
        val expected = 5
    in
        assert 
        $ officiate (deck, moves, goal) = expected 
        $ "officiate: game stops on empty deck even if there are moves"
    end

val () =
    let
        val deck = [(Club, Jack), (Club, Num 3), (Club, Num 10), (Club, Jack)]
        val moves = [Draw, Draw, Draw, Draw]
        val goal = 19
        val expected = 6
    in
        assert 
        $ officiate (deck, moves, goal) = expected 
        $ "officiate: game stops on sum > goal"
    end

val () =
    let
        val deck = [(Club, Jack), (Club, Num 3), (Heart, Num 10), (Club, Jack)]
        val moves = [Draw, Discard (Club, Jack), Draw, Draw]
        val goal = 19
        val expected = 6
    in
        assert 
        $ officiate (deck, moves, goal) = expected 
        $ "officiate: player can discard cards"
    end

val 0 =
    let
        val deck = [(Club, Jack), (Club, Num 3), (Heart, Num 10), (Club, Jack)]
        val moves = [Discard (Club, Jack), Draw, Draw, Draw]
        val goal = 19
        val expected = 6
    in
        officiate (deck, moves, goal)
            handle IllegalMove =>
                assert true "officiate: raises exception on missing card"
                |> (fn _ => 0)
    end

val () = complete ()