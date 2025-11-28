% Factos male
male(frank).
male(jay).
male(javier).
male(merle).
male(phil).
male(mitchell).
male(joe).
male(manny).
male(cameron).
male(pameron).
male(bo).
male(dylan).
male(luke).
male(rexford).
male(george).
male(calhoun).

% Factos female
female(grace).
female(dede).
female(gloria).
female(barb).
female(claire).
female(haley).
female(alex).
female(lily).
female(poppy).

% Factos parent
parent(grace, phil).
parent(frank, phil).

parent(dede, claire).
parent(dede, mitchell).
parent(jay, claire).
parent(jay, mitchell).
parent(jay, joe).

parent(gloria, joe).
parent(gloria, manny).
parent(javier, manny).

parent(barb, cameron).
parent(barb, pameron).
parent(merle, cameron).
parent(merle, pameron).

parent(pameron, calhoun).
parent(bo, calhoun).

parent(phil, haley).
parent(phil, alex).
parent(phil, luke).
parent(claire, haley).
parent(claire, alex).
parent(claire, luke).

parent(mitchell, lily).
parent(mitchell, rexford).
parent(cameron, lily).
parent(cameron, rexford).

parent(dylan, george).
parent(dylan, poppy).
parent(haley, george).
parent(haley, poppy).

%rules

father(X,Y):- parent(X,Y) , male(X).

grandparent(X,Y):- parent(X,Z) , parent(Z,Y).

grandmother(X,Y):- parent(X,Z) , parent(Z,Y), female(X).

siblings(X,Y):- parent(P1,X) , parent(P2,X), P1 \== P2,
                parent(P1,Y), parent(P2,Y),
                X \== Y.

halfsiblings(X,Y):- parent(Z,X) , parent(Z,Y), X \== Y,
                \+ siblings(X,Y).


cousins(X,Y):- parent(P_X, X), parent(P_Y,Y),
                siblings(P_X,P_Y),
                X \== Y.

uncle(X,Y):- parent(P1, Y), male(X) ,siblings(X,P1).


ancestor_of(X, Y) :-
                parent(X, Y).

ancestor_of(X, Y) :-
                parent(X, Z),
                ancestor_of(Z, Y).

descendant_of(X, Y) :-
                ancestor_of(Y, X).




get_married_years(X,Y,Year) :- married(X,Y,Year).
get_married_years(X,Y,Year) :- married(Y,X,Year).

get_divorced_years(X,Y,Year) :- divorced(X,Y,Year).
get_divorced_years(X,Y,Year) :- divorced(Y,X,Year).

marriage_years(X, Y, Years) :-
                            get_married_years(X,Y,Mar).
                            get_divorced_years(X,Y,Div).

                            Years is Div - Mar.


born(jay, 1946-5-23). 
born(claire, 1970-11-13). 
born(mitchell, 1973-7-10).


before(X, Y) :- X @< Y.


older(X, Y, Older) :- born(X,YearX),
                    born (Y,YearY),
                    before(YearX,YearY),
                    Older = X.

older(X, Y, Older) :- born(X,YearX),
                    born (Y,YearY),
                    before(YearY,YearX),
                    Older = Y.


oldest(X) :- born(X,DateX), not (born(Y,YearY), before(YearX, YearY)).  % this works because of backtracking,
    % if prolog selects a person and it turns out that person is not the oldest it backtracks and tries another candidate.