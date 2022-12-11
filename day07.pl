% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(dcg/basics), [integer/3]).
:- use_module(library(yall)).
:- use_module(library(clpfd)).

command(cd(Dir)) -->
    "cd ",
    string_without("\n", D), { string_codes(Dir, D) }.

command(ls) --> "ls".

dir_entry(dir(Name)) -->
    "dir ",
    string_without("\n", N), { string_codes(Name, N) },
    "\n".

dir_entry(file(Name, Size)) -->
    integer(Size),
    " ",
    string_without("\n", N), { string_codes(Name, N) },
    "\n".

dir_entries([Ent | Ents]) -->
    dir_entry(Ent),
    !,
    dir_entries(Ents).
dir_entries([]) --> [].

command_with_output(command(Command, Output)) -->
    "$ ", !,
    command(Command),
    "\n",
    (  { Command = ls }
    -> dir_entries(Output)
    ;  { Output = [] }
    ).

commands([Command | Commands]) -->
    command_with_output(Command),
    !,
    commands(Commands).
commands([]) --> [].

%%%

cd(state(T, _), "/", state(T, ["/"])) :- !.
cd(state(T, [D]), "..", state(T, [D])) :- !.
cd(state(T, [_ | Ds]), "..", state(T, Ds)).
cd(state(T, Pwd), Dir, state(T, [Dir | Pwd])).

ls(state(T, Pwd), Entries, state(T1, Pwd)) :-
    foldl({Pwd}/[Entry, I, O] >> (
              (  (file(Name, Size) = Entry)
              -> O = [[Name | Pwd] - Size | I]
              ;  I = O
              )
          ),
         Entries,
         T,
         T1).

run_command(command(cd(Dir), _), State, State1) :-
    cd(State, Dir, State1).
run_command(command(ls, Output), State, State1) :-
    ls(State, Output, State1).

run_commands(Commands, State) :-
    foldl(run_command, Commands, state([], "/"), State), !.

total_directory_size(Tree, Dir, Total) :-
    aggregate_all(
        sum(Size),
        (
            member(Path - Size, Tree),
            append(_, Dir, Path)
        ),
        Total
    ).

directories(Tree, Dirs) :-
    maplist([([_ | D] - _), D] >> true, Tree, Leaves),
    findall(Dir, (member(Leaf, Leaves), append(_, Dir, Leaf), Dir \= []), Dupes),
    sort(Dupes, Dirs).

total_at_most_100K(Tree, Total) :-
    directories(Tree, Dirs),
    maplist(total_directory_size(Tree), Dirs, Sizes),
    include([X]>>(X #=< 100000), Sizes, SizesAtMost100K),
    sum_list(SizesAtMost100K, Total).

solution_part1(File, Solution) :-
    phrase_file(commands(Commands), File),
    !,
    run_commands(Commands, state(Tree, _)),
    total_at_most_100K(Tree, Solution).

%%%

best_directory(Tree, Best) :-
    directories(Tree, Dirs),
    total_directory_size(Tree, ["/"], TotalUsed),
    Unused #= 70000000 - TotalUsed,
    Needed #= 30000000 - Unused,
    maplist(total_directory_size(Tree), Dirs, Sizes),
    include({Needed}/[Size]>>(Size #>= Needed), Sizes, Candidates),
    sort(Candidates, [Best | _]).

solution_part2(File, Solution) :-
    phrase_file(commands(Commands), File),
    !,
    run_commands(Commands, state(Tree, _)),
    best_directory(Tree, Solution).

:- begin_tests(day7).

test(ls_only_dirs) :-
    run_command(
        command(ls, [dir("a"), dir("b")]),
        state([], ["/"]),
        state([], ["/"])
    ).

test(cd_beyond_slash) :-
    run_command(
        command(cd(".."), []),
        state([], ["/"]),
        state([], ["/"])
    ).

test(sample_input) :-
    solution_part1("sample.input", 95437).

:- end_tests(day7).
