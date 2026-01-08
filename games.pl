
% state(MissionariesLeft, CannibalsLeft, BoatPosition)
% move (Missionaries,Cannibals)
possible_move(1,0).
possible_move(0,1).
possible_move(2,0).
possible_move(0,2).
possible_move(1,1).

move(state(M, C, left), move(M_Move, C_Move), state(NewM, NewC, right)) :-
            possible_move(M_Move,C_Move),
            NewM is M - M_Move,
            NewC is C - C_Move,

            NewM>=0, NewC >= 0.

move(state(M, C, right), move(M_Move, C_Move), state(NewM, NewC, left)) :-  % since we are moving right then we add people
            possible_move(M_Move,C_Move),
            NewM is M + M_Move,
            NewC is C + C_Move,

            NewM>=3, NewC >= 3.


safe(state(M,C,_)) :-
            (
            M >=C
            ;
            M == 0
            ),
            MRight is 3-M,
            CRight is 3-C,
            (
            MRight >=CRight
            ;
            MRight == 0
            ),
            
solve(state(0,0,right),_,[]).

solve(State,Visited,[Move|Moves]) :-
            move(State,Move,NextState),
            safe(NextState),
            not(member(NextState,Visited)),

            solve(NextState,[NextState|Visited],Moves).


missionares_and_cannibals(Moves) :-
            StartState = state(3,3,left),
            solve(StartState,[StartState],Moves).


steps(Steps, N, L) :-
            findall(Path, climb(Steps,Path), L),
            length(L, N).
            
climb(0,[]).

climb(Steps,[1|Path]) :- 
            Steps > 0,
            NewSteps is Steps - 1,
            climb(NewSteps,Path).

climb(Steps,[2|Path]) :-
            Steps > 1, 
            NewSteps is Steps -2,
            climb(NewSteps,Path).





