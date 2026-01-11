

map(_, [], []).
map(Pred,[H|T],[R|Result]) :-
    call(Pred,H,R),
    map(Pred,T,Result).

fold(_,FinalValue,[],FinalValue).
fold(Pred, StartValue, [H|T], FinalValue) :-
    call(Pred,H,StartValue,NewValue),
    fold(Pred,NewValue,T,FinalValue).


separate([],_,[],[]).
separate([H|T], Pred, [H|Yes], No) :-
    call(Pred,H),!,
    separate(T,Pred,Yes,No).
separate([H|T],Pred,Yes,[H|No]) :-
    separate(T,Pred,Yes,No).

take_while(_,[],[],[]).
take_while(Pred, [H|T], [H|Front], Back) :-
    call(Pred,H),!,
    take_while(Pred,T,Front,Back).
take_while(_,Back,[],Back).


% node(Value, Left, Right)
tree_map(_,null,null).
tree_map(Pred, node(X,Left,Right), node(Y,YLeft,YRight)) :-
    call(Pred,X,Y),!,
    tree_map(Pred,Left,YLeft),
    tree_map(Pred,Right,YRight).


%dfs_tree(Tree,Value,CurrentLevel,Level)

dfs_tree(node(Val,_,_),Val,Level,Level).
dfs_tree(node(X,L,R),Val,CurrentLevel,Level) :-
    NextLevel is CurrentLevel + 1,
    (
    dfs_tree(L,Val,NextLevel,Level);
    dfs_tree(R,Val,NextLevel,Level)
    ).


:- op(400,fx,append).
:- op(410,xfx,to).
:- op(420,xfx,results_in).

append A to B results_in C :-
    append(A,B,C).


