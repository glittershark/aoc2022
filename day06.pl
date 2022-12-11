% -*- mode: prolog -*-

start_marker(Buf, Len) :-
    prefix(Chars, Buf),
    length(Chars, Len),
    list_to_set(Chars, Chars).

ends_with_start_marker(Buf, Len) :-
    reverse(Buf, Rev),
    start_marker(Rev, Len).

start_marker_position(Input, MarkerLen, Position) :-
    prefix(Prefix, Input),
    ends_with_start_marker(Prefix, MarkerLen),
    !,
    length(Prefix, Position).

solution_part1(File, Solution) :-
    open(File, read, Stream),
    read_string(Stream, _, String),
    string_chars(String, Chars),
    start_marker_position(Chars, 4, Solution).

solution_part2(File, Solution) :-
    open(File, read, Stream),
    read_string(Stream, _, String),
    string_chars(String, Chars),
    start_marker_position(Chars, 14, Solution).
