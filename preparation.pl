square_rec(N, S) :-
    square_rec(N,N,S).

square_rec(0,_,0).
square_rec(N,X,S) :-
    N >0,
    N1 is N -1,
    square_rec(N1,X,S1),
    S is X + S1.

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N, F) :-
    N1 is N-1,
    N2 is N-2,
    fibonacci(N1,F1),
    fibonacci(N2,F2),
    F is F1+F2.


collatz(1,0).

% if it's even
collatz(N, S) :-
    even(N),
    !,
    NextStep is N//2,
    collatz(NextStep,S1),
    S is 1 + S1.

% if it's odd
collatz(N, S) :-
    NextStep is 3*N + 1,
    collatz(NextStep,S1),
    S is 1 + S1.

is_prime(X) :- is_prime(X,2).

is_prime(X,X).

is_prime(X,CurN) :-
    CurN > 1,
    CurN < X,
    !,
    X mod CurN =\= 0,
    NextN is CurN +1,
    is_prime(X,NextN).

gcd(X,0,X).

gcd(X, Y, G) :-
    Y =\= 0,
    Next is X mod Y,
    gcd(Y,Next,G).
    

