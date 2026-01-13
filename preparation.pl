

rotate(List, Position, Rotations, NewList) :-
    append(Prefix,[ListToRotate|Suffix],List),
    Index is Position - 1,
    length(Prefix,Index),
    rotate_list(Rotations,ListToRotate,Rotated),
    append(Prefix,[Rotated|Suffix],NewList).
    
matches([],[]).

matches([[H|_]|Rest], [H|Keys]) :-
    matches(Rest,Keys).



solve(Code, Key, States) :-
    bfs([[Code]],Key,States).

bfs([[Current|Path]|_],Key,States) :-
    matches(Current,Key),!,
    reverse([Current|Path],States).

bfs([[Current|Path]|RestQ],Key,States) :-
    findall([Next,Current|Path],(move(Current,Next), \+ (member(Next,[Current|Path]))),NextPaths),
    append(RestQ,NextPaths,UpdatedQ),
    bfs(UpdatedQ,Key,States).




% gives_gift_to(Giver, Gift, Receiver)
gives_gift_to(bernardete, 'The Exchange', celestina).
gives_gift_to(celestina, 'The Brethren', eleuterio).
gives_gift_to(eleuterio, 'The Summons', felismina).
gives_gift_to(felismina, 'River God', juvenaldo).
gives_gift_to(juvenaldo, 'Seventh Scroll', leonilde).
gives_gift_to(leonilde, 'Sunbird', bernardete).
gives_gift_to(marciliano, 'Those in Peril', nivaldo).
gives_gift_to(nivaldo, 'Vicious Circle', sandrino).
gives_gift_to(sandrino, 'Predator', marciliano).



circle_size(Person, Size) :-
    gives_gift_to(Person,_,Next),
    dfs(Next,Person,[Next],Path),
    length(Path, Size).
    

dfs(Dest,Dest,Visited,Visited).

dfs(Current,Destination,Visited,Path) :-
    gives_gift_to(Current,_,Next),
    \+ (member(Next,Visited)),
    dfs(Next,Destination,[Next|Visited],Path).

bfs([[Dest|Path]|_],Dest,[Dest|Path]).

bfs([[Current|Path]|RestQ],Dest,Path) :-
    findall([Next,Current|Path],(gives_gift_to(Current,_,Next),\+ (member(Next,[Current|Path]))),NewPaths),
    append(RestQ,NewPaths,UpdatedQ),
    bfs(UpdatedQ,Dest,Path).


largest_circle(People) :-
    findall(N-Sorted,(gives_gift_to(Person,_,Next), dfs(Next,Person,[Next],Path), sort(Path,Sorted), length(Sorted,N)),L),
    sort(L,List),
    last(List,Max-_),
    member(Max-People,List).


