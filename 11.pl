parse_input(File) :-
    read_file_to_string(File, Content, []),
    split_string(Content, "\n", "", Lines0),
    exclude(=(""), Lines0, Lines),
    maplist(assert_line, Lines).

assert_device_output(Device, Output) :-
    assertz(device_output(Device, Output)).

assert_line(Line) :-
    split_string(Line, ": ", "", Parts),
    [Device, _ | Outputs] = Parts,
    maplist(assert_device_output(Device), Outputs).

sum_pairs([], 0, 0).
sum_pairs([[A, B] | Rest], SumA, SumB) :-
    sum_pairs(Rest, RestSumA, RestSumB),
    SumA is RestSumA + A,
    SumB is RestSumB + B.

:- dynamic cached_result/4.

paths("out", Dac/Fft, 1, Task2) :-
    Dac == seen,
    Fft == seen
    -> Task2 = 1
    ;  Task2 = 0.

paths(Current, Flags, Task1, Task2) :-
    cached_result(Current, Flags, Task1, Task2), !.

paths(Current, Dac/Fft, Task1, Task2) :-
    ((Dac == seen ; Current == "dac") -> Dac1 = seen ; Dac1 = unseen),
    ((Fft == seen ; Current == "fft") -> Fft1 = seen ; Fft1 = unseen),
    findall([Task1, Task2], (device_output(Current, Next),
                             paths(Next, Dac1/Fft1, Task1, Task2)),
            Pairs),
    sum_pairs(Pairs, Task1, Task2),
    assertz(cached_result(Current, Dac/Fft, Task1, Task2)).

paths(Current, Task1, Task2) :-
    paths(Current, unseen/unseen, Task1, Task2).

:-
    parse_input("input/11.txt"),
    paths("you", Task1, _),
    paths("svr", _, Task2),
    format("~d ~d~n", [Task1, Task2]).
