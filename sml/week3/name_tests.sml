use "name.sml";
use "test.sml";

val () =
    assert 
        (all_except_option ("str", []) = NONE)
        "all_except_option: NONE on empty list"

val () =
    assert 
        (all_except_option ("str", ["str"]) = SOME [])
        "all_except_option: filters out matched string"

val () =
    assert 
        (all_except_option ("str", ["str", "bar"]) = SOME ["bar"])
        "all_except_option: filters only necessary elements"

val () =
    assert 
        (all_except_option ("str", ["str", "bar", "str"]) = SOME ["bar"])
        "all_except_option: filters all matches"

val () =
    assert 
        (all_except_option ("str", ["str", "bar", "baz"]) = SOME ["bar", "baz"])
        "all_except_option: preserve order"

val () =
    let
        val substitutions = [
            ["Fred","Fredrick"],
            ["Elizabeth","Betty"],
            ["Freddie","Fred","F"]
        ]
        val name = "Fred"
        val expect = ["Fredrick","Freddie","F"]
    in
        assert 
            (get_substitutions1 (substitutions, name) = expect)
            "get_substitutions1: collects substitutions"
    end

val () =
    let
        val substitutions = [
            ["Fred","Fredrick"],
            ["Jeff","Jeffrey"],
            ["Geoff","Jeff","Jeffrey"]
        ]
        val name = "Jeff"
        val expect = ["Jeffrey","Geoff","Jeffrey"]
    in
        assert 
            (get_substitutions1 (substitutions, name) = expect)
            "get_substitutions1: collect duplicates"
    end

val () =
    let
        val substitutions = [
            ["Fred","Fredrick"],
            ["Jeff","Jeffrey"],
            ["Geoff","Jeff","Jeffrey"]
        ]
        val name = "Jeff"
        val expect = ["Jeffrey","Geoff","Jeffrey"]
        val res1 = get_substitutions1 (substitutions, name)
        val res2 = get_substitutions2 (substitutions, name) 
    in
        assert 
            (res1 = res2)
            "get_substitutions2: works as previous one"
    end

val () =
    let
        val substitutions = [
            ["Fred","Fredrick"],
            ["Elizabeth","Betty"],
            ["Freddie","Fred","F"]
        ]
        val full_name = {first="Fred", middle="W", last="Smith"}
        val expect = [
            {first="Fred", last="Smith", middle="W"},
            {first="Fredrick", last="Smith", middle="W"},
            {first="Freddie", last="Smith", middle="W"},
            {first="F", last="Smith", middle="W"}
        ]
    in
        assert
            (similar_names (substitutions, full_name) = expect)
            "similar_names: creates correct list"
    end

val () = complete ()