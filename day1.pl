% -*- mode: prolog -*-
:- use_module(utils).
:- use_module(library(readutil)).

group_lines(Lines, [Group | Groups]) :-
    append(Group, ["" | Rest], Lines),
    !,
    group_lines(Rest, Groups).
group_lines(Lines, [Lines]).

group_elf(Group, Elf) :-
    maplist(number_string, Elf, Group).

read_input(File, Elves) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    group_lines(Lines, Groups),
    maplist(group_elf, Groups, Elves).

%%%

solution_part1(File, Solution) :-
    read_input(File, Elves),
    maplist(sum_list, Elves, Totals),
    max_list(Totals, Solution),
    !.


solution_part2(File, Solution) :-
    read_input(File, Elves),
    maplist(sum_list, Elves, Totals),
    sort(Totals, Asc),
    reverse(Asc, Desc),
    prefix(Top3, Desc),
    length(Top3, 3),
    sum_list(Top3, Solution),
    !.
