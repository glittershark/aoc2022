% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(clpfd)).
:- use_module(library(readutil)).

rucksack_compartments(Rucksack, C1 - C2) :-
    append(C1, C2, Rucksack),
    length(Rucksack, TotalLength),
    TotalLength #= CompartmentLength * 2,
    length(C1, CompartmentLength),
    length(C2, CompartmentLength).

common_item(C1 - C2, Common) :-
    member(Common, C1),
    member(Common, C2).

priority(Item, Priority) :-
    char_code(Item, Code),
    (  % a-z
       Code in 97..122
    -> Priority #= Code - 96
       % A-Z
    ;  Code in 65..90
    -> Priority #= Code - 65 + 27
    ).

read_input(File, Rucksacks) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(string_chars, Lines, Rucksacks).

%%%

solution_part1(File, Solution) :-
    read_input(File, Rucksacks),
    maplist(rucksack_compartments, Rucksacks, Compartments),
    maplist(common_item, Compartments, CommonItems),
    maplist(priority, CommonItems, Priorities),
    sum_list(Priorities, Solution),
    !.

%%%

badge([E1, E2, E3], Badge) :-
    member(Badge, E1),
    member(Badge, E2),
    member(Badge, E3),
    !.

solution_part2(File, Solution) :-
    read_input(File, Rucksacks),
    chunks(3, Rucksacks, Groups),
    maplist(badge, Groups, Badges),
    maplist(priority, Badges, Priorities),
    sum_list(Priorities, Solution).
