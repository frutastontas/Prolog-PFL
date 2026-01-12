
:- use_module(library(lists)).

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

next_to(X,X,_). 
next_to(X,Y,[A,B,C,D,E,F]) :- 
    consecutive(X,Y,[A,B,C,D,E,F]). 
next_to(X,Y,[A,B,C,D,E,F]) :- 
    consecutive(Y,X,[A,B,C,D,E,F]). 

consecutive(X,Y,Board) :-  
    append(Prefix,[X,Y|Suffix], Board). 

anywhere(X, Board) :-
    member(X,Board).

one_space(X, Y, Board) :-
    (
        append(_,[X,_,Y|_],Board);
        append(_,[Y,_,X|_],Board)
    ).

across(X, Y, [A,B,C,D,E,F]) :-
    Bottom = [A,B],
    Upper = [D,E,F],
    (
        (member(X,Bottom),member(Y,Upper));
        (member(Y,Bottom),member(X,Upper))
    ).

same_edge(X, Y, [A,B,C,D,E,F]) :-
    Bottom = [A,B],
    Upper = [D,E,F],
    (
        (member(X,Bottom),member(Y,Bottom));
        (member(Y,Upper),member(X,Upper))
    ).

position(X, [Pos|Positions], Board) :-
    nth1(Pos,Board,X).

position(X, [_|Positions], Board) :-
    position(X,Positions,Board).


solve(Constraints, Board) :-
    length(List, 6),
    call_constraints(Constraints,List),
    ValidBoard = [green, yellow, blue, orange, white, black],
    permutation(List, ValidBoard),
    Board = List.
    

call_constraints([],Board).
call_constraints([Constraint|Constraints],Board) :-
    call(Constraint,Board),
    call_constraints(Constraints,Board).


best_score(Constraints, Score) :-
    ValidBoard = [green, yellow, blue, orange, white, black],
    findall(S, (permutation(ValidBoard,Board), calculate_score(Constraints,Board,0,S)), List),
    sort(List,Sorted),
    reverse(Sorted, Reversed),
    Reversed = [Score|_].
    
    

calculate_score([],Board,Score,Score).

calculate_score([Constraint|Constraints],Board,CurrentScore,Score) :-
    call(Constraint,Board),!,
    calculate_score(Constraints,Board,CurrentScore,Score).

calculate_score([_|Constraints],Board,CurrentScore,Score) :-
    NewScore is CurrentScore - 1,
    calculate_score(Constraints,Board,NewScore,Score).