% -*- mode: prolog -*-
:- use_module(utils).
:- use_module(library(readutil)).
:- use_module(library(clpfd)).

decode("A", rock).
decode("B", paper).
decode("C", scissors).

decode("X", rock).
decode("Y", paper).
decode("Z", scissors).

beats(rock, scissors).
beats(scissors, paper).
beats(paper, rock).

outcome(A, B, win) :- beats(A, B).
outcome(A, A, draw).
outcome(B, A, loss) :- beats(A, B).

score(loss, 0).
score(draw, 3).
score(win, 6).

shape_score(rock, 1).
shape_score(paper, 2).
shape_score(scissors, 3).

round_score(round(Opponent, Us, Outcome), Score) :-
    shape_score(Us, ShapeScore),
    outcome(Us, Opponent, Outcome),
    score(Outcome, OutcomeScore),
    Score #= ShapeScore + OutcomeScore.

%%%

parse_line_part1(Line, Round) :-
    split_string(Line, " ", "", Chars),
    maplist(decode, Chars, [Opponent, Us]),
    Round = round(Opponent, Us, _).

read_input_part1(File, Rounds) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(parse_line_part1, Lines, Rounds).

solution_part1(File, Solution) :-
    read_input_part1(File, Rounds),
    maplist(round_score, Rounds, Scores),
    sum_list(Scores, Solution).

%%%

decode_outcome_part2("X", loss).
decode_outcome_part2("Y", draw).
decode_outcome_part2("Z", win).

parse_line_part2(Line, Round) :-
    split_string(Line, " ", "", [OpponentS, OutcomeS]),
    decode(OpponentS, Opponent),
    decode_outcome_part2(OutcomeS, Outcome),
    Round = round(Opponent, _, Outcome).

read_input_part2(File, Rounds) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    maplist(parse_line_part2, Lines, Rounds).

solution_part2(File, Solution) :-
    read_input_part2(File, Rounds),
    maplist(round_score, Rounds, Scores),
    sum_list(Scores, Solution).
