% -*- mode: prolog -*-

:- use_module(library(dcg/basics), [integer/3]).
:- use_module(library(clpfd)).

instruction(noop).
instruction(addx(_)).

cycles(addx(_), 2).
cycles(noop, 1).

interpret(noop, X, X).
interpret(addx(Operand), X1, X2) :-
    X2 #= X1 + Operand.

%%% Parsing

instruction(noop) --> "noop\n".
instruction(addx(Operand)) -->
    "addx ",
    integer(Operand),
    "\n".

instructions([I | Is]) -->
    instruction(I),
    !,
    instructions(Is).
instructions([]) --> [], !.

%%%

initial_state(Program, state(Program, 1, 1, 1)).

step(
    state([Instruction | Is], X1, IC1, Cycle1),
    state(Program, X2, IC2, Cycle2)
) :-
    Cycle2 #= Cycle1 + 1,
    (  cycles(Instruction, IC1)
    -> interpret(Instruction, X1, X2),
       IC2 = 1,
       Program = Is
    ;  X1 #= X2,
       IC2 #= IC1 + 1,
       Program = [Instruction | Is]
    ).

done(state([], _, _, _)).

state_cycle(state(_, _, _, Cycle), Cycle).

signal_strength(state(_, X, _, Cycle), Strength) :-
    Strength #= X * Cycle.

%%%

interesting_cycle(20).
interesting_cycle(N) :- 0 #= (N - 20) mod 40.

step_saving_result(
    State1 - Res1,
    State2 - Res2
) :-
    state_cycle(State1, Cycle),
    (  interesting_cycle(Cycle)
    -> signal_strength(State1, Strength),
       Res2 #= Res1 + Strength, writeln(Cycle - Strength)
    ;  Res2 #= Res1
    ),
    step(State1, State2).

run_until_done_saving_result(State - Res, State - Res) :- done(State), !.
run_until_done_saving_result(S1, S_omega) :-
    step_saving_result(S1, S2),
    run_until_done_saving_result(S2, S_omega).

sum_interesting_cycles(Program, Sum) :-
    initial_state(Program, State),
    run_until_done_saving_result(State - 0, _ - Sum).

solution_part1(File, Solution) :-
    phrase_file(instructions(Program), File), !,
    sum_interesting_cycles(Program, Solution).

%%%

x_within_sprite(X, X).
x_within_sprite(X, Sprite) :-
    ( X #= Sprite - 1
    ; X #= Sprite + 1
    ).

position_within_sprite(X - _, Sprite) :-
    % Our positions are 1-based, Sprites are 0-based
    Sprite_1 #= Sprite + 1,
    x_within_sprite(X, Sprite_1).

cycle_position(Cycle, X - Y) :-
    X #= ((Cycle - 1) mod 40) + 1,
    Y #= (Cycle div 40) + 1.

write_matrix(M1, X - Y, Val, M2) :-
    nth1(Y, M1, Row1),
    replace1(Row1, X, Val, Row2),
    replace1(M1, Y, Row2, M2).

maybe_draw(state(_, X, _, Cycle), CRT1, CRT2) :-
    cycle_position(Cycle, Pos),
    (  position_within_sprite(Pos, X)
    -> write_matrix(CRT1, Pos, '#', CRT2)
    ;  CRT2 = CRT1
    ).

initial_crt(CRT) :-
    length(CRT, 6),
    length(Row, 40),
    maplist(=('.'), Row),
    maplist(=(Row), CRT).

render_crt(CRT, Rendered) :-
    maplist(atomics_to_string, CRT, Rows),
    string_lines(Rendered, Rows).

step_with_crt(
    State1 - CRT1,
    State2 - CRT2
) :-
    maybe_draw(State1, CRT1, CRT2),
    step(State1, State2).

run_until_done_with_crt(State - CRT, State - CRT) :- done(State), !.
run_until_done_with_crt(S1, S_omega) :-
    step_with_crt(S1, S2),
    run_until_done_with_crt(S2, S_omega).

run_part2(File) :-
    phrase_file(instructions(Program), File), !,
    initial_state(Program, State),
    initial_crt(CRT1),
    run_until_done_with_crt(State - CRT1, _ - CRT),
    render_crt(CRT, Rendered),
    write(Rendered).
