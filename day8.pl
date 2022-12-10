% -*- mode: prolog -*-

:- use_module(library(clpfd)).
:- use_module(library(readutil)).
:- use_module(library(yall)).

direction(left).
direction(right).
direction(up).
direction(down).

height(Grid, X - Y, Height) :-
    nth0(Y, Grid, Row),
    nth0(X, Row, Height).

in_direction(left, X1 - Y, X2 - Y) :-
    X1 #< X2.
in_direction(right, X1 - Y, X2 - Y) :-
    X1 #> X2.
in_direction(up, X - Y1, X - Y2) :-
    Y1 #< Y2.
in_direction(down, X - Y1, X - Y2) :-
    Y1 #> Y2.

within_grid(Grid, X - Y) :-
    X #>= 0, Y #>= 0,
    length(Grid, Ylen),
    [Row | _] = Grid,
    length(Row, Xlen),
    X #< Xlen, Y #< Ylen.

sort_positions(left, P1, P2)  :- sort(1, @>, P1, P2).
sort_positions(right, P1, P2) :- sort(1, @<, P1, P2).
sort_positions(up, P1, P2)    :- sort(2, @>, P1, P2).
sort_positions(down, P1, P2)  :- sort(2, @<, P1, P2).

label_position(X - Y) :- label([X, Y]).

line_of_sight(Dir, Pos, Grid, LOS) :-
    findall(
        Pos1,
        (
            in_direction(Dir, Pos1, Pos),
            within_grid(Grid, Pos1),
            label_position(Pos1)
        ),
        Positions
    ),
    sort_positions(Dir, Positions, Sorted),
    maplist(height(Grid), Sorted, LOS).

visible(Dir, Pos, Grid) :-
    height(Grid, Pos, OurHeight),
    line_of_sight(Dir, Pos, Grid, LOS),
    forall(
        member(Height, LOS),
        Height #< OurHeight
    ).

visible(Pos, Grid) :-
    direction(Dir),
    visible(Dir, Pos, Grid).

read_grid(File, Grid) :-
    open(File, read, Stream),
    read_string(Stream, _, String),
    concat(Trimmed, "\n", String),
    split_string(Trimmed, "\n", "", Lines),
    maplist(string_codes, Lines, Codes),
    maplist(maplist([Code, Num]>>(Num #= Code - 48)), Codes, Grid).

solution_part1(File, Solution) :-
    read_grid(File, Grid),
    aggregate_all(count, Pos, visible(Pos, Grid), Solution).

%%%

viewing_distance(Dir, Pos, Grid, Dist) :-
    height(Grid, Pos, OurHeight),
    line_of_sight(Dir, Pos, Grid, LOS),
    prefix(Visible, LOS),
    ( (last(Visible, Blocker),
       Blocker #>= OurHeight
      )
    ; Visible = LOS ),
    length(Visible, Dist),
    !.

scenic_score(Grid, Pos, Score) :-
    viewing_distance(left, Pos, Grid, Left),
    viewing_distance(right, Pos, Grid, Right),
    viewing_distance(up, Pos, Grid, Up),
    viewing_distance(down, Pos, Grid, Down),
    Score #= Left * Right * Up * Down.

solution_part2(File, Solution) :-
    read_grid(File, Grid),
    findall(Pos, (within_grid(Grid, Pos), label_position(Pos)), Positions),
    maplist(scenic_score(Grid), Positions, Scores),
    max_list(Scores, Solution).

:- begin_tests(day8).

sample_grid(
    [[3,0,3,7,3],
     [2,5,5,1,2],
     [6,5,3,3,2],
     [3,3,5,4,9],
     [3,5,3,9,0]]
).

test(line_of_sight) :-
    sample_grid(Grid),

    line_of_sight(left, 1-1, Grid, [2]),
    line_of_sight(left, 2-1, Grid, [5, 2]),

    line_of_sight(right, 2-1, Grid, [1, 2]),
    line_of_sight(right, 1-2, Grid, [3, 3, 2]),

    line_of_sight(up, 1-2, Grid, [5, 0]),
    line_of_sight(down, 1-2, Grid, [3, 5]).

test(visible, [nondet]) :-
    sample_grid(Grid),

    visible(1-1, Grid).

test(viewing_distance) :-
    sample_grid(Grid),
    viewing_distance(up, 2-1, Grid, 1),
    viewing_distance(left, 2-1, Grid, 1),
    viewing_distance(right, 2-1, Grid, 2),
    viewing_distance(down, 2-1, Grid, 3),

    viewing_distance(right, 1-3, Grid, 1).

test(scenic_score) :-
    sample_grid(Grid),
    scenic_score(Grid, 2-1, 4),
    scenic_score(Grid, 2-3, 8),

    scenic_score(Grid, 1-3, 1).

test(part2) :-
    solution_part2("sample.input", 8).

:- end_tests(day8).
