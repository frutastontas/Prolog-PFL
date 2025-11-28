

list_size(List, Size) :-list_size_rec(List,Size).

list_size_rec([],0).    
list_size_rec([_|T],Size) :-
                        list_size_rec(T,Size1),
                        Size is Size1 + 1.

list_sum([], 0).

list_sum([H|T],Sum) :-
                    list_sum(T,Sum1),
                    Sum is Sum1 + H.

inner_product([],[],0).
inner_product ([H1|T1], [H2|T2], Result) :-
                                         inner_product(T1,T2,Result1),
                                         Result is (H1 * H2) + Result1.



count(X, [], 0).

count(X,[X|T],N) :-
                 count(X,T,N1),
                 N is N1 +1.

count(X,[H|T],N) :-
                X\=H,
                count(X,T,N).
                 

invert(List1, List2) :- invert_aux(List1,[],List2).

invert_aux([],Acc,Acc).
invert_aux([H|T],Acc,L2) :-
                      invert_aux(T,[H|Acc],L2).



del_one(_, [], []).
del_one(X, [X|T], T) :- !.
del_one(X, [H|T], [H|Result]) :-
    del_one(X, T, Result).




del_all(_, [], []).

del_all(X,[X|T],Result) :- del_all(X,T,Result).
del_all(X,[H|T],[H|Result]) :-
    X\=H,
    del_all(X,T,Result).



%del_all_list(+ListElems, +List1, ?List2)
del_all_list([],Result,Result).
del_all_list([H1|T1], Lista1, Lista2) :- 
                                     del_all(H1,Lista1,ListaFiltrada),
                                     del_all_list(T1,ListaFiltrada,Lista2).


%del_dups(+List1, ?List2)
del_dups([], []).

del_dups([H|T], [H|Result]) :-
    del_all(H, T, CleanedTail),
    del_dups(CleanedTail, Result).
                       