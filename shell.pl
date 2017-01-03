:- module(shell, [main/0]).
:- use_module(library(dcg/basics)).
:- use_module(parse).

parse_command(Codes, Command) :-
    phrase(parse_command(Command), Codes).

parse_command(load(Filename)) -->
    "l", whites, string(StringFilename), { atom_codes(Filename, StringFilename) }.
%parse_command(add_note(Note)) -->
%    "n", whites, string(Note).
parse_command(add_ingredient(i(Qty, Unit, Name))) -->
    "a", whites, number(Qty), whites, string(Unit), whites, string(Name).
parse_command(change_ingredient(i(Qty, Unit, Name))) -->
    "c", whites, number(Qty), whites, string(Unit), whites, string(Name).

execute_command(_, load(Filename), ready(loaded(Filename), Recipe)) :-
    load_recipe(Filename, Recipe).
execute_command(empty, _, empty) :-
    format('No recipe loaded~n').
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

print_state(empty) :-
    format('No recipe loaded.~n~n').
print_state(state(ready(_, Recipe))) :-
    print_recipe(Recipe).

print_recipe(recipe(Name, Ingredients)) :-
    format('%s~n~n', Name),
    forall(member(i(Qty, Unit, Name), Ingredients),
           format('  ~s ~s ~s~n', Qty, Unit, Name)).

main :-
    print_state(empty), main(empty).

main(State) :-
    read_command(Command),
    execute_command(State, Command, NewState),
    format('~w~n', NewState),
    print_state(NewState),
    main(NewState).
