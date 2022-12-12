% -*- mode: prolog -*-

:- use_module(utils).
:- use_module(library(record)).
:- use_module(library(dcg/basics), [integer/3]).
:- use_module(library(yall)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).

:- record monkey(
       number:integer,
       items:list(integer),
       operation,
       test,
       if_true_throw_to,
       if_false_throw_to,
       activity:integer=0
   ).

%%% Parsing

monkey(Monkey) -->
    "Monkey ", integer(Number), ":\n",
    "  Starting items: ", items(Items), "\n",
    "  Operation: ", operation(Operation), "\n",
    "  Test: ", test(Test), "\n",
    "    If true: throw to monkey ", integer(IfTrueThrowTo), "\n",
    "    If false: throw to monkey ", integer(IfFalseThrowTo), "\n",
    {
        make_monkey([ number(Number),
                      items(Items),
                      operation(Operation),
                      test(Test),
                      if_true_throw_to(IfTrueThrowTo),
                      if_false_throw_to(IfFalseThrowTo)
                    ],
                    Monkey)
    }.

items([I1, I2 | Is]) -->
    integer(I1),
    ", ", !,
    items([I2 | Is]).
items([I]) --> integer(I), !.
items([]) --> [], !.

operator(*) --> "*".
operator(+) --> "+".

expr(op(Lhs, Op, Rhs)) -->
    base_expr(Lhs),
    " ",
    operator(Op),
    " ",
    base_expr(Rhs).
base_expr(old) --> "old".
base_expr(lit(Num)) --> integer(Num).

operation(Operation) -->
    "new = ",
    expr(Operation).

test(divisible(Num)) -->
    "divisible by ",
    integer(Num).

monkeys([M1, M2 | Ms]) -->
    monkey(M1),
    "\n", !,
    monkeys([M2 | Ms]).
monkeys([M]) --> monkey(M), !.
monkeys([]) --> [], !.

read_monkeys(File, Monkeys) :-
    phrase_file(monkeys(MonkeyList), File, ""),
    maplist(
        [Monkey, MonkeyNum - Monkey] >> monkey_number(Monkey, MonkeyNum),
        MonkeyList,
        MonkeyPairs
    ),
    list_to_assoc(MonkeyPairs, Monkeys).

%%% Execution

run_op(*, X, Y, R) :- R is X * Y.
run_op(+, X, Y, R) :- R is X + Y.

eval(old, Old, Old).
eval(lit(Num), _, Num).
eval(op(Lhs, Op, Rhs), Old, Res) :-
    eval(Lhs, Old, LhsV),
    eval(Rhs, Old, RhsV),
    run_op(Op, LhsV, RhsV, Res).

test(divisible(Num), Val) :-
     0 is Val mod Num.

inspect(Part, Monkey, Item1, ThrowTo - Item) :-
    monkey_operation(Monkey, Operation),
    eval(Operation, Item1, Item2),
    (  Part = part1
    -> Item is Item2 div 3
    ;  Item = Item2
    ),
    monkey_test(Monkey, Test),
    (  test(Test, Item)
    -> monkey_if_true_throw_to(Monkey, ThrowTo)
    ;  monkey_if_false_throw_to(Monkey, ThrowTo)
    ).

monkey_turn(Part, Monkey1, Monkey2, Throws) :-
    monkey_items(Monkey1, Items),
    maplist(inspect(Part, Monkey1), Items, Throws),
    length(Items, Inspections),
    monkey_activity(Monkey1, Activity1),
    Activity2 is Activity1 + Inspections,
    set_activity_of_monkey(Activity2, Monkey1, Monkey2).

give_monkey_item(MonkeyNum - Item, Monkeys1, Monkeys) :-
    del_assoc(MonkeyNum, Monkeys1, Monkey1, Monkeys2),
    monkey_items(Monkey1, Items1),
    set_items_of_monkey([Item | Items1], Monkey1, Monkey),
    put_assoc(MonkeyNum, Monkeys2, Monkey, Monkeys).

monkey_nums(Monkeys, Nums) :- assoc_to_keys(Monkeys, Nums).

turn(Part, Num, Monkeys1, Monkeys) :-
    del_assoc(Num, Monkeys1, Monkey1, Monkeys2),
    monkey_turn(Part, Monkey1, Monkey2, Throws),
    set_items_of_monkey([], Monkey2, Monkey3),
    put_assoc(Num, Monkeys2, Monkey3, Monkeys3),
    foldl(give_monkey_item, Throws, Monkeys3, Monkeys).

round(Part, Monkeys1, Monkeys2) :-
    monkey_nums(Monkeys1, Nums),
    foldl(turn(Part), Nums, Monkeys1, Monkeys2).

rounds(_, 0, Monkeys, Monkeys).
rounds(Part, N, Monkeys1, Monkeys) :-
    round(Part, Monkeys1, Monkeys2),
    N1 is N - 1,
    rounds(Part, N1, Monkeys2, Monkeys).

write_items(Monkeys) :-
    map_assoc(
        [Monkey] >> (
            monkey_number(Monkey, Number),
            monkey_items(Monkey, Items),
            maplist(number_string, Items, ItemStrings),
            format("Monkey ~w: ~w~n", [Number, ItemStrings])
        ),
        Monkeys).

write_activity(Monkeys) :-
    map_assoc(
        [Monkey] >> (
            monkey_number(Monkey, Number),
            monkey_activity(Monkey, Activity),
            format("Monkey ~w inspected items ~w times.~n", [Number, Activity])
        ),
        Monkeys
    ).

monkey_business(Monkeys, Business) :-
    assoc_to_values(Monkeys, Ms),
    maplist(monkey_activity, Ms, Activities),
    sort(0, @>=, Activities, [A1, A2 | _]),
    Business is A1 * A2.

%%%

solution_part1(File, Solution) :-
    read_monkeys(File, Monkeys),
    rounds(part1, 20, Monkeys, Monkeys1),
    write_items(Monkeys1),
    write_activity(Monkeys1),
    monkey_business(Monkeys1, Solution).

solution_part2(File, Solution) :-
    read_monkeys(File, Monkeys),
    rounds(part2, 21, Monkeys, Monkeys1),
    write_items(Monkeys1),
    write_activity(Monkeys1),
    monkey_business(Monkeys1, Solution).
