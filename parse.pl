:- use_module(library(sgml)).

%% the primary structure of interest here is shaped like this:
%% recipe(Title, Ingredients)
%% where Title is an atom or string, and Ingredients is a list of these ingredient structures:
%% i(Qty, Unit, Name)

ingredient_xml(element(i, Attrs, [Name]),
               i(Qty, Unit, Name)) :-
    member(qty=QtyA, Attrs),
    atom_number(QtyA, Qty),
    member(unit=Unit, Attrs).

recipe_xml(element(recipe, [title=Title], Body),
           recipe(Title, Ingredients)) :-
    Body = [element(ingredients, [], IngredientList)],
    maplist(ingredient_xml, IngredientList, Ingredients).

load_recipe(Filename, Recipe) :-
    load_xml(Filename, [Structure], [space(remove)]),
    recipe_xml(Structure, Recipe).

scale_recipe(recipe(Title, UnscaledIngredients), Scale, recipe(Title, ScaledIngredients)) :-
    maplist(scale_ingredient(Scale), UnscaledIngredients, ScaledIngredients).

scale_ingredient(Scale, i(Qty, Unit, Name), i(QtyScaled, Unit, Name)) :-
    QtyScaled is Qty * Scale.

    
                                                     
