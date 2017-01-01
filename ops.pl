:- module(ops, [scale_recipe/2]).

scale_recipe(recipe(Title, UnscaledIngredients),
             Scale,
             recipe(Title, ScaledIngredients)) :-
    maplist(scale_ingredient(Scale), UnscaledIngredients, ScaledIngredients).

scale_ingredient(Scale, i(Qty, Unit, Name), i(QtyScaled, Unit, Name)) :-
    QtyScaled is Qty * Scale.
