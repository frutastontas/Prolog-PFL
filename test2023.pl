% dish(Name, Price, IngredientGrams).
dish(pizza,2200, [cheese-300, tomato-350]).
dish(ratatouille,2200, [tomato-70, eggplant-150, garlic-50]).
dish(garlic_bread,  1600, [cheese-50, garlic-200]).


% ingredient(Name, CostPerGram).
ingredient(cheese,   
4).
ingredient(tomato,   
2).
ingredient(eggplant, 7).
ingredient(garlic,   
6).


count_ingredients(Dish, NumIngredients) :-
    dish(Dish,_,IngredientGrams),
    length(IngredientGrams, NumIngredients).

ingredient_amount_cost(Ingredient, Grams, TotalCost) :-
    ingredient(Ingredient,CostPerGram),
    TotalCost is CostPerGram * Grams.



dish_profit(Dish, Profit) :-
    dish(Dish,Price,IngredientGrams),
    dish_cost(IngredientGrams,TotalCost),
    Profit is Price - TotalCost.

dish_cost([],0).
dish_cost([Ingredient-Grams|Ingredients],TotalCost) :-
    ingredient_amount_cost(Ingredient,Grams,Cost),
    dish_cost(Ingredients,RestCost),
    TotalCost is Cost + RestCost.

% My dish is the most expensive if there is no other dish with a bigger price
most_expensive_dish(Dish, Price) :-
    dish(Dish,Price,_),
    \+ ((dish(Dish2,Price2,_), Price2 > Price)). % por negação


consume_ingredient(IngredientStocks, Ingredient, Grams, NewIngredientStocks) :-
    append(Prefix,[Ingredient-Amount|Suffix],IngredientStocks),
    NewAmount is Amount - Grams,
    append(Prefix,[Ingredient-NewAmount|Suffix],NewIngredientStocks).


count_dishes_with_ingredient(Ingredient, N) :-
    count_dishes_with_ingredient(Ingredient,[],N).


count_dishes_with_ingredient(Ingredient,Visited,N) :-
    dish(Dish,_,Ingredients),
    \+ (member(Dish,Visited)),
    member(Ingredient-_,Ingredients),
    !,
    count_dishes_with_ingredient(Ingredient,[Dish|Visited],N1),
    N is 1 + N1.

count_dishes_with_ingredient(_,_,0).

extract_ingredients([],[]).
extract_ingredients([Ingredient-_|IngredientGrams],[Ingredient|Ingredients]) :-
    extract_ingredients(IngredientGrams,Ingredients).
    
list_dishes(DishIngredients) :-
    findall(Dish-ListOfIngredients, (dish(Dish,_,IngredientGrams),extract_ingredients(IngredientGrams,ListOfIngredients)), DishIngredients).


myreverse([],Reversed,Reversed).
myreverse([H|T],Acc,Reversed) :-
    myreverse(T,[H|Acc],Reversed).


extract_dishes([],[]).
extract_dishes([_-Dish|Tail],[Dish|Dishes]) :-
    extract_dishes(Tail,Dishes).

most_lucrative_dishes(Dishes) :-
    setof(Profit-Dish,Price^IngredientGrams^(dish(Dish,Price,IngredientGrams),dish_profit(Dish,Profit)),List),
    myreverse(List,[],DecreasingList),
    extract_dishes(DecreasingList,Dishes).

    