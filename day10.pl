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
