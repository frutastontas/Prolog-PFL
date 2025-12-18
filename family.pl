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





children(Person, Children) :- findall(Child, parent(Person,Child), Children).


children_of([],[]).
children_of([Person|T], [Person-Children|Result]) :-
                                            children(Person,Children),
                                            children_of(T,Result).


family(F) :- setof(Member, (parent(Member,_);parent(_,Member)), F).


couple(X-Y) :-
        parent(X,C),
        parent(Y,C),
        X \= Y.

couples(List) :- setof(Couple,couple(Couple),List).

spouse_children(Person, SC) :-
                            couple(Person-Spouse),
                            setof(Child, (parent(Person,Child), parent(Spouse,Child)), Children),
                            SC = Spouse\Children.


parents_of_two(Parents) :-
    setof(Parent, 
          Children^(setof(Child, parent(Parent, Child), Children),
                    length(Children, N), 
                    N >= 2), 
          Parents).
                            