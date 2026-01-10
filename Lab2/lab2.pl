

%%%%%%%%%% Examples

%% 12 solutions 
example(1, [ next_to(white,orange), 
             next_to(black,black), 
             across(yellow,orange), 
             next_to(green,yellow), 
             position(blue,[1,2,6]), 
             across(yellow,blue) ]). 
 
%% 1 solution 
example(2, [ across(white,yellow), 
             position(black,[1,4]), 
             position(yellow,[1,5]), 
             next_to(green, blue), 
             same_edge(blue,yellow), 
             one_space(orange,black) ]). 
 
%% no solutions (5 constraints are satisfiable) 
example(3, [ across(white,yellow), 
             position(black,[1,4]), 
             position(yellow,[1,5]), 
             same_edge(green, black), 
             same_edge(blue,yellow), 
             one_space(orange,black) ]). 
 
%% same as above, different order of constraints 
example(4, [ position(yellow,[1,5]), 
             one_space(orange,black), 
             same_edge(green, black), 
             same_edge(blue,yellow), 
             position(black,[1,4]), 
             across(white,yellow) ]).




%%%%%%%%%%%%%%%%%%

%Board([A,B,C,D,E,F])

next_to(X,X,_). 
next_to(X,Y,[A,B,C,D,E,F]) :- 
consecutive(X,Y,[A,B,C,D,E,F]). 
next_to(X,Y,[A,B,C,D,E,F]) :- 
consecutive(Y,X,[A,B,C,D,E,F]).

consecutive(X,Y,Board) :-  
append(Prefix,[X,Y|Suffix], Board).


at_ends(X, [X,B,C,D,E,F]).
at_ends(X,[A,B,C,D,E,X]).

not_next_to(X, Y, Board) :-
    \+(append(_,[X,Y|_],Board);append(_,[Y,X|_],Board)).

distance(X, Y, N, Board) :-
    nth1(XPosition,Board,X),
    nth1(YPosition,Board,Y),
    Distance is XPosition - YPosition,
    N = Distance.

distance(X, Y, N, Board) :-
    nth1(XPosition,Board,X),
    nth1(YPosition,Board,Y),
    Distance is YPosition - XPosition,
    N = Distance.   

anywhere(X, Board) :-
    member(X, Board).

one_space(X,X,_).
one_space(X, Y, Board) :-
    append(_,[X,_,Y|_],Board).

one_space(X,Y,Board) :-
    append(_,[Y,_,X|_],Board).
    

position(X, [Pos|_], Board) :-
    nth1(Pos,Board,X). % See if X is in the position specified by Pos

% If not then go to this predicate to continue the search in next position
position(X,[_|Positions],Board) :-
    position(X,Positions,Board).
    
across(X,X,_).
across(X, Y, [A,B,C,D,E,F]) :-
    SubList1 = [A,B],
    SubList2 = [D,E,F],
    (
        (member(X,SubList1), member(Y,SubList2))
        ;
        (member(Y,SubList1), member(X,SubList2))
    ).

same_edge(X,X,_).
same_edge(X, Y, [A,B,C,D,E,F]) :-
    SubList1 = [A,B],
    SubList2 = [D,E,F],
    (
        (member(X,SubList1), member(Y,SubList1))
        ;
        (member(X,SubList2), member(Y,SubList2))
    ).




solve(Constraints,Board) :-
    length(List, 6), % Create a list with 6 slots to be filled
    process_constraints(Constraints,List),
    ListOfColors = [green, yellow, blue, orange, white, black], % generate a list of the colors to check if all the slots on the board were filled
    permutation(ListOfColors, List),    % if it is a permutation it means that all the constraints worked and it created a filled board
    Board = List.
    
    
process_constraints([],Board).
process_constraints([Constraint|Constraints], Board) :-
    call(Constraint,Board),
    process_constraints(Constraints,Board).


best_score(Constraints, Score) :-
    ListOfColors = [green, yellow, blue, orange, white, black],
    setof(S,Perm^(permutation(ListOfColors,Perm), calculate_score(Constraints,Perm,0,S)),Scores),
    last(Scores, Score).
    



calculate_score([],_,Score,Score).

calculate_score([Constr|Constraints],Board,CurrentScore,Score) :-
    call(Constr,Board),!,
    calculate_score(Constraints,Board,CurrentScore,Score). %Current score stays the same

calculate_score([_|Constraints],Board,CurrentScore,Score) :-
    NewScore is CurrentScore -1,
    calculate_score(Constraints,Board,NewScore,Score). %Current score goes down