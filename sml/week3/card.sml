use "list.sml";

datatype color = Black | Red
datatype rank = Ace | Queen | King | Jack | Num of int
datatype suit = Club | Diamond | Heart | Spade
type card = suit * rank
datatype move = Draw | Discard of card

exception IllegalMove

type card_color = card -> color
val card_color: card_color = fn card =>
    case card of
       (Club, _) => Black
     | (Spade, _) => Black
     | (Diamond, _) => Red
     | (Heart, _) => Red

type card_value = card -> int
val card_value: card_value = fn (_, rank) =>
    case rank of
    Num num => num
    | Ace => 11
    | _ => 10

type remove_card = card list * card * exn -> card list
val remove_card: remove_card = fn (cards, to_remove, exp) =>
    let
        fun filter card (found, acc) =
            if card <> to_remove orelse found
            then (found, card :: acc)
            else (true orelse found, acc)
    in
        case fold filter (false, []) cards of
        (true, filtered) => filtered
        | (false, _) => raise exp
    end

(* not tail recursive *)
type all_same_color = card list -> bool
val rec all_same_color: all_same_color = fn cards =>
    case cards of
    [] => true
    | one :: [] => true
    | first :: second :: rest =>
        card_color first = card_color second andalso all_same_color rest

type sum_cards = card list -> int
val sum_cards: sum_cards = fn cards =>
    cards |> fold (fn card => fn sum => card_value card + sum) 0 

(* even shorter via partial application :) *)
val sum_cards: sum_cards = fold (fn card => fn sum => card_value card + sum) 0 

type score = card list * int -> int
val score: score = fn (cards, goal) =>
    let
        val sum = sum_cards cards
        val preliminary_score = 
            if sum < goal 
            then goal - sum 
            else 3 * (sum - goal)  
        val same_color = all_same_color cards
    in
        if same_color then preliminary_score div 2 else preliminary_score
    end 

type officiate = card list * move list * int -> int
val officiate: officiate = fn (deck, moves, goal) =>
    let
        fun draw (deck, hand) =
            case deck of
                [] => (deck, hand)
                | head :: tail => 
                    if sum_cards hand > goal
                    then (deck, hand)
                    else (tail, head :: hand)
        
        fun play move (deck, hand) = 
            case move of
            Draw => draw (deck, hand) 
            | Discard card => (deck, remove_card (hand, card, IllegalMove))
        
        fun round hand = score (hand, goal)
    in
        moves |> fold play (deck, []) |> #2 |> round 
    end