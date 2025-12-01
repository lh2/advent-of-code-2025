parse_move(['L'|Rest], N) :-
    number_chars(N0, Rest),
    N is -N0.

parse_move(['R'|Rest], N) :-
    number_chars(N, Rest).

parse_input(File, Moves) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "", Lines0),
    exclude(=(""), Lines0, Lines),
    maplist(string_chars, Lines, CharLists),
    maplist(parse_move, CharLists, Moves).

passed_zero_left(0, _, Q, Count) :- Count is Q - 1.
passed_zero_left(_, 0, Q, Count) :- Count is Q + 1.
passed_zero_left(_, _, Q, Q).

passed_count(Move, Pos, NewPos, Q, Count) :-
    Move < 0,
    passed_zero_left(Pos, NewPos, Q, Count).

passed_count(_, _, _, Q, Q).

move_dial(Pos, N, NewPos, Passed0Count) :-
    T is Pos + N,
    divmod(T, 100, Q0, NewPos),
    Q is abs(Q0),
    passed_count(N, Pos, NewPos, Q, Passed0Count).

solve(_, [], 0, 0).
solve(Pos, [Move|Rest], Task1, Task2) :-
    move_dial(Pos, Move, NewPos, T2R1),
    ( NewPos =:= 0 -> T1R1 = 1 ; T1R1 = 0 ),
    solve(NewPos, Rest, T1R2, T2R2),
    Task1 is T1R1 + T1R2,
    Task2 is T2R1 + T2R2.

solve(Moves, Task1, Task2) :-
    solve(50, Moves, Task1, Task2).

:-
    parse_input("input/01.txt", Moves),
    solve(Moves, Task1, Task2),
    format("~d ~d~n", [Task1, Task2]).
