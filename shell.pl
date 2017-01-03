:- module(shell, [main/0, print_recipe/1]).
:- use_module(library(dcg/basics), except([atom//1])).
:- use_module(parse).

parse_command(Codes, Command) :-
    phrase(parse_command(Command), Codes).

atom(A) --> string(String), { atom_codes(A, String) }.

sep --> white, whites.

parse_command(load(Filename)) -->
    "l", sep, atom(Filename).
%parse_command(add_note(Note)) -->
%    "n", whites, string(Note).
parse_command(add_ingredient(i(Qty, Unit, Name))) -->
    "a", sep, number(Qty), sep, atom(Unit), sep, atom(Name).
parse_command(change_ingredient(i(Qty, Unit, Name))) -->
    "c", sep, number(Qty), sep, atom(Unit), sep, atom(Name).
parse_command(save) --> "s".
parse_command(not_understood(Text), Text, []).



execute_command(State, not_understood(Text), State) :-
    format('Error: I don''t understand this: ~s~n', [Text]).
execute_command(_, load(Filename), ready(loaded(Filename), Recipe)) :-
    load_recipe(Filename, Recipe).
execute_command(empty, _, empty) :-
    format('Error: no active recipe.~n').
execute_command(Recipe, save, Recipe) :-
    ready(loaded(Filename), Recipe) = Recipe,
    write_recipe(Filename, Recipe),
    format('Saved.~n').
execute_command(ready(Meta, recipe(Title, Ingredients)),
                add_ingredient(Ingredient),
                ready(Meta, recipe(Title, NewIngredients))) :-
    append(Ingredients, [Ingredient], NewIngredients).
execute_command(ready(Meta, recipe(Title, Ingredients)),
                change_ingredient(i(Qty, Unit, Name)),
                ready(Meta, recipe(Title, NewIngredients))) :-
    select(i(_, _, Name), Ingredients, i(Qty, Unit, Name), NewIngredients).

read_command(Command) :-
    read_line_to_codes(user_input, Codes),
    parse_command(Codes, Command).

print_state(empty)            :-  format('No recipe loaded.~n~n').
print_state(ready(_, Recipe)) :-  print_recipe(Recipe).

print_recipe(recipe(Recipe, Ingredients)) :-
    format('~w~n~n', [Recipe]),
    forall(member(i(Qty, Unit, Name), Ingredients),
           format('  ~w ~w ~w~n', [Qty, Unit, Name])).

main :-
    print_state(empty), main(empty).

main(State) :-
    read_command(Command),
    execute_command(State, Command, NewState),
    print_state(NewState),
    main(NewState).
