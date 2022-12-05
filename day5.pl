% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(clpfd)).
:- use_module(library(dcg/high_order)).
:- use_module(library(dcg/basics)).
:- use_module(library(aggregate)).
:- use_module(library(yall)).

crate(no_crate) --> "   ", !.
crate(crate(Name)) -->
    "[",
    [NameCode],
    "]",
    { char_code(Name, NameCode), ! }.

crate_row_sep --> " ".

crate_row(CrateRow) -->
    sequence(crate, crate_row_sep, CrateRow).

crate_rows([CR|CRs]) -->
  crate_row(CR),
  "\n",
  !,
  crate_rows(CRs).
crate_rows([]) --> [].

crate_number(N) --> " ", digit(Code), { N #= Code - 48 }.
crate_number_row(Ns) --> sequence(crate_number, "  ", Ns), !.


crate_diagram(diagram(CrateRows, Numbers)) -->
    crate_rows(CrateRows),
    crate_number_row(Numbers),
    "\n".

move(move(Num, From, To)) -->
    "move ",
    integer(Num),
    " from ",
    integer(From),
    " to ",
    integer(To).

moves([Move | Moves]) -->
    move(Move),
    "\n",
    !,
    moves(Moves).
moves([]) --> [].

drawing(drawing(CrateRows, Moves)) -->
    crate_diagram(diagram(CrateRows, _)),
    "\n",
    moves(Moves).

normalize_crate_rows(CrateRows, Normalized) :-
    aggregate(max(L, R), (member(R, CrateRows), length(R, L)), max(MaxLen, _)),
    maplist(
        ({MaxLen}/[Input, Padded] >> (
             length(Padded, MaxLen),
             (  length(Input, MaxLen)
             -> (Padded = Input)
             ;  append(Input, Padding, Padded),
                maplist([X] >> (X = no_crate), Padding)
             )
         )),
        CrateRows,
        Normalized
    ).

rows_stacks(CrateRows, CrateStacks) :-
    normalize_crate_rows(CrateRows, Normalized),
    transpose(Normalized, Stacks1),
    maplist(exclude([X] >> (X = no_crate)), Stacks1, CrateStacks).

apply_move_part1(move(N, _, _), CrateStacks, CrateStacks) :-
    N #= 0.

apply_move_part1(move(N, From, To), CrateStacks, Result) :-
    length(CrateStacks, Len),
    length(Result1, Len),

    foreach(
        (
            between(1, Len, Pos),
            Pos =\= From,
            Pos =\= To,
            nth1(Pos, CrateStacks, S)
        ),
        nth1(Pos, Result1, S)
    ),

    nth1(From, CrateStacks, [Crate | FromStack]),
    nth1(From, Result1, FromStack),

    nth1(To, CrateStacks, ToStack),
    nth1(To, Result1, [Crate | ToStack]),

    N1 #= N - 1,
    apply_move_part1(move(N1, From, To), Result1, Result).

apply_moves_part1(CrateStacks, Moves, Result) :-
    foldl(apply_move_part1, Moves, CrateStacks, Result).

solution_part1(File, Solution) :-
    phrase_file(drawing(drawing(CRs, Moves)), File),
    !,
    rows_stacks(CRs, CrateStacks),
    apply_moves_part1(CrateStacks, Moves, Result),
    maplist([[crate(H) | _], H] >> (true), Result, Top),
    maplist(char_code, Top, Codes),
    string_codes(Solution, Codes).

%%%

apply_move_part2(move(N, From, To), CrateStacks, Result) :-
    length(CrateStacks, Len),
    length(Result, Len),

    foreach(
        (
            between(1, Len, Pos),
            Pos =\= From,
            Pos =\= To,
            nth1(Pos, CrateStacks, S)
        ),
        nth1(Pos, Result, S)
    ),

    nth1(From, CrateStacks, FromStack),
    append(Crates, Remaining, FromStack),
    length(Crates, N),
    nth1(From, Result, Remaining),

    nth1(To, CrateStacks, ToStack),
    append(Crates, ToStack, ToStackResult),
    nth1(To, Result, ToStackResult).

apply_moves_part2(CrateStacks, Moves, Result) :-
    foldl(apply_move_part2, Moves, CrateStacks, Result).

solution_part2(File, Solution) :-
    phrase_file(drawing(drawing(CRs, Moves)), File),
    !,
    rows_stacks(CRs, CrateStacks),
    apply_moves_part2(CrateStacks, Moves, Result),
    maplist([[crate(H) | _], H] >> (true), Result, Top),
    maplist(char_code, Top, Codes),
    string_codes(Solution, Codes).
