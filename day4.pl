% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(clpfd)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics), [integer/3]).

section_assignment(X - Y) -->
    integer(X),
    "-",
    integer(Y).

assignment_pair(A1 - A2) -->
    section_assignment(A1),
    ",",
    section_assignment(A2).

parse_line(Line, AP) :-
    string_phrase(assignment_pair(AP), Line).

read_input(File, Assignments) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(parse_line, Lines, Assignments).

write_assignments(File, Assignments) :-
    maplist(parse_line, Lines, Assignments),
    open(File, write, Stream),
    foreach(member(Line, Lines), writeln(Stream, Line)).

fully_contained((X1 - Y1) - (X2 - Y2)) :-
    X1 #=< X2,
    Y1 #>= Y2.

fully_contained((X1 - Y1) - (X2 - Y2)) :-
    X2 #=< X1,
    Y2 #>= Y1.

solution_part1(File, Solution) :-
    read_input(File, Assignments),
    include(fully_contained, Assignments, Res),
    length(Res, Solution).

%%%

overlaps((X1 - Y1) - (X2 - Y2)) :-
    X1 #=< X2,
    Y1 #>= X2.

overlaps((X1 - Y1) - (X2 - Y2)) :-
    X2 #=< X1,
    Y2 #>= X1.

solution_part2(File, Solution) :-
    read_input(File, Assignments),
    include(overlaps, Assignments, Res),
    length(Res, Solution).
