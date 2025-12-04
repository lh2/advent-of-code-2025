parse_input(File) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "", Lines0),
    exclude(=(""), Lines0, Lines),
    length(Lines, Length),
    numlist(1, Length, Ys),
    maplist(assert_line, Lines, Ys).

assert_line(Line, Y) :-
    string_chars(Line, Chars),
    assert_chars(Chars, 1, Y).

assert_chars([], _, _).
assert_chars([C|Rest], X, Y) :-
    assert_field(C, X, Y),
    X2 is X + 1,
    assert_chars(Rest, X2, Y).

assert_field('@', X, Y) :-
    assertz(roll(X, Y)).
assert_field(_, _, _).

forklift_access(X, Y) :-
    roll(X, Y),
    neighbour_count(X, Y, N),
    N < 4.

neighbour_offset(DX, DY) :-
    between(-1, 1, DX),
    between(-1, 1, DY),
    \+ (DX = 0, DY = 0).

has_neighbour(X, Y, DX, DY) :-
    neighbour_offset(DX, DY),
    NX is X + DX,
    NY is Y + DY,
    roll(NX, NY).

neighbour_count(X, Y, N) :-
    aggregate_all(count, has_neighbour(X, Y, _, _), N).

task1(N) :-
    aggregate_all(count, forklift_access(_, _), N).

task2_(C0, Total) :-
    findall(roll(X, Y), forklift_access(X, Y), List),
    maplist(retract, List),
    length(List, C1),
    C3 is C0 + C1,
    ( C1 = 0
    -> Total is C3
    ;  task2_(C3, Total)
    ).

task2(N) :-
    task2_(0, N).

:-
    parse_input("input/04.txt"),
    task1(Task1),
    task2(Task2),
    format("~d ~d~n", [Task1, Task2]).
