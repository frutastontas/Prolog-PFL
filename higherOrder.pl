
map(_,[],[]).
map(Pred, [H1|T1],[R|Result]) :-
        call(Pred,H1,R),
        map(Pred,T1,Result).

fold(_,FinalValue,[],FinalValue).
fold(Pred, StartValue, [H|T], FinalValue) :-
        call(Pred,H,StartValue,NextValue),
        fold(Pred,NextValue,T,FinalValue).


separate([],_,[],[]).
separate([H|T], Pred, [H|Yes], No) :-
        call(Pred,H),
        !,
        separate(T,Pred,Yes,No).

separate([H|T], Pred, Yes, [H|No]) :-
        separate(T,Pred,Yes,No).



take_while(Pred,[],[],[]).
take_while(Pred, [H|T], [H|Front], Back) :-
        call(Pred,H),
        !,
        take_while(Pred,T,Front,Back).

take_while(_,[H|T],[],[H|T]).


ask_execute :-
    write('Insert the goal to execute'),
    read(Goal),
    callable(Goal),
    call(Goal).


    
my_functor(Term, Name, Arity)  :-
        var(Term),
        !,
        length(Args,Arity), % Args is a list with basically nothing but it has size Arity
        Term =.. [Name|Args].

my_functor(Term,Name,Arity) :-
        Term =.. [Name|Args],
        length(Args,Arity).


my_arg(Index, Term, Arg) :-
        nonvar(Term),
        Term =.. [Name|Args],
        append(Prefix,[Arg|_],Args),
        Before is Index-1,
        length(Prefix,Before).
        
op(700, xfx, univ).    


% node(Value, Left, Right)
tree_size(null,0).
tree_size(node(_,L,R), Size) :-
        tree_size(L,SizeL),
        tree_size(R,SizeR),
        Size is 1+SizeL+SizeR.

tree_map(_,null,null).
tree_map(Pred, node(X,L,R), node(Y,NewL,NewR)) :-
        call(Pred,X,Y),
        tree_map(Pred,L,NewL),
        tree_map(Pred,R,NewR).


tree_value_at_level(Tree, Value, Level) :-
        var(Value),
        nonvar(Level),
        !,
        tree_value_at_level_Level(Tree,Value,0,Level).

tree_value_at_level(Tree,Value,Level) :-
        tree_value_at_level_Value(Tree,Value,0,Level).

tree_value_at_level(Tree,Value,Level) :- Level is -1.



tree_value_at_level_Value(node(Value,L,R), Value, Level,Level).

tree_value_at_level_Value(node(V,L,R), Value, CurrLevel,Level):-
        NextLevel is CurrLevel +1,
        (
        tree_value_at_level_Value(L,Value,NextLevel,Level);
        tree_value_at_level_Value(R,Value,NextLevel,Level)
        ).



tree_value_at_level_Level(node(Value,_,_), Value, Level,Level).

tree_value_at_level_Level(node(V,L,R), Value, CurrLevel,Level):-
        NextLevel is CurrLevel +1,
        (
        tree_value_at_level_Level(L,Value,NextLevel,Level);
        tree_value_at_level_Level(R,Value,NextLevel,Level)
        ).


op(700, xfx, exists_in).    

Element exists_in List :- exists(List,Element).



exists([Element|T],Element) :- !.

exists([_|T],Element) :-
        exists(T,Element).


    