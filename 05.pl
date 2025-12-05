parse_input(File) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "", Lines0),
    exclude(=(""), Lines0, Lines),
    maplist(parse_line, Lines).

parse_line(Line) :-
    split_string(Line, "-", "", Parts),
    assert_value(Parts).

assert_value([From0, To0]) :-
    number_string(From, From0),
    number_string(To, To0),
    assertz(fresh_range(From, To)).
assert_value([Id0]) :-
    number_string(Id, Id0),
    assertz(ingredient(Id)).

merge_fresh_ranges([fresh_range(From, To)], [fresh_range(From, To)]).
merge_fresh_ranges([fresh_range(From1, To1), fresh_range(From2, To2) | Rest], Merged) :-
    ( From2 =< To1 + 1
    -> NewTo is max(To1, To2),
       merge_fresh_ranges([fresh_range(From1, NewTo) | Rest], Merged)
    ;  merge_fresh_ranges([fresh_range(From2, To2) | Rest], MergedRest),
       Merged = [fresh_range(From1, To1) | MergedRest]
    ).

merge_fresh_ranges() :-
    findall(fresh_range(From, To), fresh_range(From, To), Ranges0),
    maplist(retract, Ranges0),
    sort(Ranges0, Ranges1),
    merge_fresh_ranges(Ranges1, Ranges),
    maplist(assertz, Ranges).

ingredient_fresh(Id) :-
    ingredient(Id),
    fresh_range(From, To),
    between(From, To, Id).

fresh_range_id_count(From, To, Count) :-
    fresh_range(From, To),
    Count is To - From + 1.

task1(Task1) :-
    aggregate_all(count, ingredient_fresh(_), Task1).

task2(Task2) :-
    aggregate_all(sum(Count), fresh_range_id_count(_, _, Count), Task2).

:-
    parse_input("input/05.txt"),
    merge_fresh_ranges(),
    task1(Task1),
    task2(Task2),
    format("~d ~d~n", [Task1, Task2]).
