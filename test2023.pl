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



%G1
edge(g1, br, o).
edge(g1, br, ni).
edge(g1, o, ni).
edge(g1, o, c).
edge(g1, o, h).
edge(g1, h, c).
edge(g1, h, n).
edge(g1, n, he).
edge(g1, c, he).
% G2
edge(g2, br, h).
edge(g2, br, ni).
edge(g2, h, ni).
edge(g2, h, o).
edge(g2, h, c).
edge(g2, o, c).
edge(g2, o, n).
edge(g2, n, he).
edge(g2, c, he).
edge(g2, cl, he).


common_edges(G1, G2, L) :-
    extract_edges(G1,Edges1),
    extract_edges(G2,Edges2),
    intersection(Edges1,Edges2,L).


extract_edges(G,ListOfEdges) :-
    findall(O-D,edge(G,O,D),ListOfEdges).

%intersection(Edges1,Edges2,Common)

intersection([],_,[]).
intersection([O-D|Rest],Edges2,[O-D|Common]) :-
    (
        member(O-D,Edges2);
        member(D-O,Edges2)
    ),
    !,
    intersection(Rest,Edges2,Common).

intersection([_|Rest],Edges2,Common) :-
    intersection(Rest,Edges2,Common).




common_subgraphs(G1, G2, Subgraphs) :-
    common_edges(G1,G2,L),
    nodes_from_edges(L,Nodes),
    find_components(Nodes,L,Subgraphs).


nodes_from_edges(Edges, Nodes) :-
    setof(X, A^B^(member(A-B, Edges), (X=A ; X=B)), Nodes).

find_components([],_,[]).
find_components([Node|Rest],Edges,[Component|Components]) :-
    bfs([Node],Edges,[],Component),
    subtract(Rest,Component,Subtracted),
    find_components(Subtracted,Edges,Components).

% bfs(+Fila, +Arestas, +Visitados, -Resultado)

bfs([],_,Visited,Visited).

bfs([Node|RestQueue],Edges,Visited,Result) :-
    member(Node,Visited),!,
    bfs(RestQueue,Edges,Visited,Result).

bfs([Node|RestQueue],Edges,Visited,Result) :-
    nodes_neightbours(Node,Edges,Neightbours),
    append(RestQueue,Neightbours,UpdatedQueue),
    bfs(UpdatedQueue,Edges,[Node|Visited],Result).
    

nodes_neightbours(Node,Edges,Neightbours) :-
    findall(X, (member(Node-X,Edges);member(X-Node,Edges)),Neightbours).
