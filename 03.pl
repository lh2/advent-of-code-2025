:- use_module(library(list_util)).

number_char(Number, Char) :-
    number_chars(Number, [Char]).

line_bank(Line, Bank) :-
    string_chars(Line, Chars),
    maplist(number_char, Bank, Chars).

parse_input(File, Banks) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "", Lines0),
    exclude(=(""), Lines0, Lines),
    maplist(line_bank, Lines, Banks).

max_list_index([H|T], MaxI, Max) :-
    max_list_index(T, 1, H, 0, Max, MaxI).

max_list_index([], _, Max0, MaxI0, Max, MaxI) :-
    Max = Max0,
    MaxI = MaxI0.

max_list_index([H|T], I0, Max0, MaxI0, Max, MaxI) :-
    I1 is I0 + 1,
    ( H > Max0
    -> max_list_index(T, I1, H,    I0,    Max, MaxI)
    ;  max_list_index(T, I1, Max0, MaxI0, Max, MaxI)
    ).

bank_joltage_(_, _, J, 0, J).
bank_joltage_(_, 0, J, _, J).
bank_joltage_([], _, J, _, J).
bank_joltage_(Bank, N0, Joltage, Window0, Last) :-
    length(Bank, Length),
    Window is min(Length, Window0),
    split_at(Window, Bank, Search, _),
    max_list_index(Search, I, N),
    S is I + 1,
    split_at(S, Bank, _, Rest),
    RemainingWindow is Window0 - I,
    J is Last * 10 + N,
    N1 is N0 - 1,
    bank_joltage_(Rest, N1, Joltage, RemainingWindow, J).

bank_joltage(Bank, N, Joltage) :-
    length(Bank, Length),
    Window is Length - N + 1,
    bank_joltage_(Bank, N, Joltage, Window, 0).

solve([], 0, 0).
solve([Bank|Rest], Task1, Task2) :-
    bank_joltage(Bank, 2, T1Joltage0),
    bank_joltage(Bank, 12, T2Joltage0),
    solve(Rest, T1Joltage1, T2Joltage1),
    Task1 is T1Joltage0 + T1Joltage1,
    Task2 is T2Joltage0 + T2Joltage1.

:-
    parse_input("input/03.txt", Banks),
    solve(Banks, Task1, Task2),
    format("~d ~d~n", [Task1, Task2]).
