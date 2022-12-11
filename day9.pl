% -*- mode: prolog -*-

:- use_module(library(dcg/basics), [integer/3]).
:- use_module(library(clpfd)).
:- use_module(utils).

direction(left).
direction(right).
direction(up).
direction(down).

in_direction(left, X1 - Y, X2 - Y)  :- X1 #< X2.
in_direction(right, X1 - Y, X2 - Y) :- X1 #> X2.
in_direction(up, X - Y1, X - Y2)    :- Y1 #< Y2.
in_direction(down, X - Y1, X - Y2)  :- Y1 #> Y2.

move(left, X1 - Y, X2 - Y)  :- X2 #= X1 - 1.
move(right, X1 - Y, X2 - Y) :- X2 #= X1 + 1.
move(up, X - Y1, X - Y2)    :- Y2 #= Y1 - 1.
move(down, X - Y1, X - Y2)  :- Y2 #= Y1 + 1.

move(Dirs, P1, P2) :-
    is_list(Dirs), !,
    foldl(move, Dirs, P1, P2).

adjacent(P, P).
adjacent(P1, P2) :- move(_, P1, P2).
adjacent(P1, P2) :-
    move(D1, P1, Phalf),
    move(D2, Phalf, P2),
    D1 \= D2.

update_tail(state(H, T), state(H, T)) :- adjacent(H, T), !.
update_tail(state(H, T1), state(H, T2)) :-
    (  move([left, left], H, T1)
    -> move(right, T1, T2)
    ;  move([right, right], H, T1)
    -> move(left, T1, T2)
    ;  move([down, down], H, T1)
    -> move(up, T1, T2)
    ;  move([up, up], H, T1)
    -> move(down, T1, T2)

    ;  move([left, left, down], H, T1)
    -> move([right, up], T1, T2)
    ;  move([left, down, down], H, T1)
    -> move([right, up], T1, T2)

    ;  move([left, left, up], H, T1)
    -> move([right, down], T1, T2)
    ;  move([left, up, up], H, T1)
    -> move([right, down], T1, T2)

    ;  move([right, right, up], H, T1)
    -> move([left, down], T1, T2)
    ;  move([right, up, up], H, T1)
    -> move([left, down], T1, T2)

    ;  move([right, right, down], H, T1)
    -> move([left, up], T1, T2)
    ;  move([right, down, down], H, T1)
    -> move([left, up], T1, T2)
    ).

%%%

direction(left)  --> "L".
direction(right) --> "R".
direction(up)    --> "U".
direction(down)  --> "D".

parse_move(move(Dir, Count)) -->
    direction(Dir),
    " ",
    integer(Count),
    "\n".

moves([Move | Moves]) -->
    parse_move(Move),
    !,
    moves(Moves).
moves([]) --> [], !.

move_dirs(move(Dir, Count), Dirs) :-
    length(Dirs, Count),
    maplist(=(Dir), Dirs).

step(Dir, state(H1, T1), state(H2, T2)) :-
    move(Dir, H1, H2),
    update_tail(state(H2, T1), state(H2, T2)).

apply_move(Move, State1, State2) :-
    move_dirs(Move, Dirs),
    foldl(step, Dirs, State1, State2).

%%%

step_saving_tail_positions(
    Dir, State1 - Positions,
    state(H2, T2) - [T2 | Positions]
) :-
    step(Dir, State1, state(H2, T2)).

apply_move_saving_tail_positions(Move, S1, S2) :-
    move_dirs(Move, Dirs),
    foldl(step_saving_tail_positions, Dirs, S1, S2).

collect_tail_positions(
    Moves, DuplicatePositions
) :-
    foldl(
        apply_move_saving_tail_positions,
        Moves,
        state(0-0, 0-0) - [0-0],
        state(_, _) - DuplicatePositions
    ).

solution_part1(File, Solution) :-
    phrase_file(moves(Moves), File), !,
    collect_tail_positions(Moves, DuplicatePositions),
    sort(DuplicatePositions, Positions),
    length(Positions, Solution).
