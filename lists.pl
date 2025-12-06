

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

%list_perm (+L1, +L2)
list_perm([],[]).
list_perm(L1,[H|T]) :-
                del_one(H,L1,CleanedList),
                list_perm(CleanedList,T).
                       
%replicate(+Amount, +Elem, ?List)

replicate(0,_,[]).

replicate(N,X,[X|Result]) :-
                     N > 0,
                     N1 is N -1,
                     replicate(N1,X,Result).


%intersperse(+Elem, +List1, ?List2)

intersperse(_,[],[]).

intersperse(_,[X],[X]).

intersperse(C,[H|T],[H,C|Result]) :-
                                  T \= [],
                                  intersperse(C,T,Result).

%insert_elem(+Index, +List1, +Elem, ?List2)

insert_elem(0,L1,X,[X|L1]).

insert_elem(N,[H|T],X,[H|Result]) :-
                                  N > 0,
                                  N1 is N -1,
                                  insert_elem(N1,T,X,Result).



%delete_elem(+Index, +List1, ?Elem, ?List2)

delete_elem(0,[H|T],H,T).

delete_elem(N,[H|T],Elem,[H|Result]) :-
                                N > 0,
                                N1 is N -1,
                                delete_elem(N1,T,Elem,Result).


%replace(+List1, +Index, ?Old, +New, ?List2)

replace([H|T],0,H,New,[New|T]).

replace([H|T],N,Old,New,[H|Result]) :-
                            N > 0,
                            N1 is N -1,
                            replace(T,N1,Old,New,Result).


% Append function Implementations

%list_append(?L1, ?L2, ?L3).

list_append([],L2,L2).

list_append([H|T],L2,[H|Result]) :- list_append(T,L2,Result).


list_member(?Elem, ?List) :- list_append(_,[Elem|_],List).

list_last(List, Last) :- list_append(_,[Last|[]],List).

list_nth(N, List, Elem) :-
                        list_append(Prefix,[Elem|_],List), % split into suffix and prefix and see if the length of prefix is N
                        length(Prefix, N).
                        
%lists_append(+ListOfLists, ?List)
lists_append([],[]).
lists_append([XS|XSS],Result) :-
                                lists_append(XSS,R1),
                                append(XS,R1,Result).       


%list_del(+List, +Elem, ?Res)

list_del(List,Elem,Res) :-  
                        append(Prefix,[Elem|After],List),
                        append(Prefix,After,Res).

% list_before(?First, ?Second, ?List)

list_before(First,Second,List) :-
                            append(Prefix2,[Second|_],List),
                            append(_,[First|_],Prefix2).

% list_replace_one(+X, +Y, +List1, ?List2)

list_replace_one(X,Y,List1, List2) :-
                                append(Prefix,[X|AfterX],List1),
                                append(Prefix,[Y|AfterX],List2).

list_repeated(X, List) :- 
                    append(_,[X|AfterX],List),
                    append(_,[X|_],AfterX).

list_slice(List1, Index, Size, List2) :- 
                                append(Prefix1,Rest,List1),
                                length(Prefix1,Index),

                                append(List2,_,Rest),
                                length(List2,Size).


list_shift_rotate(+List1, +N, ?List2) :-
                                append(Slice,Rest,List1),
                                length(Slice,N),

                                append(Rest,Slice,List2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_to(+N, ?List) :- list_to_aux(N,1,List).


list_to_aux(N,CurN,[N]) :-
                        N =:= CurN.

list_to_aux(N,CurN,[CurN|Result]) :-
                        N =\= CurN,
                        N1 is CurN +1,
                        list_to_aux(N,N1,Result).


list_from_to(Inf, Sup, List) :- 
    list_from_to_aux(Inf, Sup, List).


list_from_to_aux(Current, Sup, []) :- 
    Current > Sup.


list_from_to_aux(Current, Sup, [Current|Result]) :-
    Current =< Sup,
    Next is Current + 1,
    list_from_to_aux(Next, Sup, Result).

list_from_to_step(+Inf, +Sup, +Step, ?List) :- list_from_to_step_aux(Inf,Sup,Step,List).

list_from_to_step_aux(Cur, Sup, Step,[]) :-
                                        Cur > Sup.

list_from_to_step_aux(Cur, Sup, Step, [Cur|Result]) :-
                                        Cur =< Sup,
                                        Next is Cur + Step,
                                        list_from_to_step_aux(Next,Sup,Step,Result).



% insert_ordered(Value, List1, List2) 

insert_ordered(Value,[],[Value]).

insert_ordered(Value, [H|T], [Value,H|T]) :-
                        H >= Value.

insert_ordered(Value, [H|T], [H|Result]) :-
                        H < Value,
                        insert_ordered(Value,T,Result).

insert_sort([],[]).
insert_sort([H|T], ?OrderedList) :-
                    insert_sort(T,SortedTail),
                    insert_ordered(H,SortedTail,OrderedList).


% rle(+List1, ?List2)


rle([],[]).

rle([H|T], [Elem|List2]) :- group(==H,T,EqualList,Rest),
                     length(EqualList,L1),
                     L is 1 + L1,
                     Elem = H-L,
                     rle(Rest,List2).


