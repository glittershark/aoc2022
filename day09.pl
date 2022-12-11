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

move(left, X1 - Y, X2 - Y)  :- X2 is X1 - 1.
move(right, X1 - Y, X2 - Y) :- X2 is X1 + 1.
move(up, X - Y1, X - Y2)    :- Y2 is Y1 - 1.
move(down, X - Y1, X - Y2)  :- Y2 is Y1 + 1.

move(Dirs, P1, P2) :-
    is_list(Dirs), !,
    foldl(move, Dirs, P1, P2).

adjacent(P, P).
adjacent(P1, P2) :- move(_, P1, P2).
adjacent(P1, P2) :-
    move(D1, P1, Phalf),
    move(D2, Phalf, P2),
    D1 \= D2.

update_tail(H, T, T) :- adjacent(H, T), !.
update_tail(H, T1, T2) :-
    (  move([left, left], H, T1)
    -> move(right, T1, T2)
    ;  move([right, right], H, T1)
    -> move(left, T1, T2)
    ;  move([down, down], H, T1)
    -> move(up, T1, T2)
    ;  move([up, up], H, T1)
    -> move(down, T1, T2)

    ;  ( move([left, left, down], H, T1)
       ; move([left, down, down], H, T1)
       ; move([left, left, down, down], H, T1)
       )
    -> move([right, up], T1, T2)

    ;  (  move([left, left, up], H, T1)
       ;  move([left, up, up], H, T1)
       ;  move([left, left, up, up], H, T1)
       )
    -> move([right, down], T1, T2)

    ;  ( move([right, right, up], H, T1)
       ; move([right, up, up], H, T1)
       ; move([right, right, up, up], H, T1)
       )
    -> move([left, down], T1, T2)

    ;  ( move([right, right, down], H, T1)
       ; move([right, down, down], H, T1)
       ; move([right, right, down, down], H, T1)
       )
    -> move([left, up], T1, T2)
    ).

update_rope([], []).
update_rope([P], [P]).
update_rope([H, T1 | Rope1], [H, T2 | Rope2]) :-
    update_tail(H, T1, T2),
    update_rope([T2 | Rope1], [T2 | Rope2]).

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

step(Dir, [H1 | T1], [H2 | T2]) :-
    move(Dir, H1, H2),
    update_rope([H2 | T1], [H2 | T2]).

apply_move(Move, Rope1, Rope2) :-
    move_dirs(Move, Dirs),
    foldl(step, Dirs, Rope1, Rope2).

%%%

step_saving_tail_positions(
    Positions,
    Dir,
    Rope1,
    Rope2
) :-
    step(Dir, Rope1, Rope2),
    last(Rope2, T),
    add_nb_set(T, Positions).

apply_move_saving_tail_positions(Positions, Move, S1, S2) :-
    move_dirs(Move, Dirs),
    foldl(step_saving_tail_positions(Positions), Dirs, S1, S2).

collect_tail_positions(
    Rope,
    Moves,
    Positions
) :-
    last(Rope, T1),
    empty_nb_set(Positions),
    add_nb_set(T1, Positions),
    foldl(
        apply_move_saving_tail_positions(Positions),
        Moves,
        Rope,
        _
    ).

initial_rope(Len, Rope) :-
    length(Rope, Len),
    maplist(=(0-0), Rope).

count_tail_positions(RopeLength, Moves, Count) :-
    initial_rope(RopeLength, Rope),
    collect_tail_positions(Rope, Moves, Positions),
    size_nb_set(Positions, Count).

%%%

solution_part1(File, Solution) :-
    phrase_file(moves(Moves), File), !,
    count_tail_positions(2, Moves, Solution).

%%%

solution_part2(File, Solution) :-
    phrase_file(moves(Moves), File), !,
    count_tail_positions(10, Moves, Solution).
