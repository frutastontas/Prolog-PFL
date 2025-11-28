
factorial(0, 1).    % base case for stoppage
factorial(N, F) :- N >0,
                N1 is N-1,
                factorial(N1,F1),   % we store the result in F1
                F is F1 * N.


sum_rec(1,1).

sum_rec(N, Sum) :-
                N > 1,
                N1 is N-1,
                sum_rec(N1, S1),
                Sum is S1 + N.

pow_rec(X,0,1).
pow_rec(X, Y, P) :-
                 Y > 0,
                 Y1 is Y-1,
                 pow_rec(X,Y1,P1),
                 P is P1 * X.




square_rec(N, S) :- square_rec_aux(N,N,S).


square_rec_aux(N,0,0).
square_rec_aux(N,X,S) :-
                      X > 0,
                      X1 is X-1,
                      square_rec_aux(N,X1,SR1),
                      S is SR1 + N.

fibonacci(0,0).
fibonacci(1,1).
fibonacci(N, F) :- N > 1,
                N1 is N -1,
                N2 is N -2,
                fibonacci(N1,F1),
                fibonacci(N2,F2),
                F is F1 + F2.




collatz(1,0).

collatz(N, S) :-
                N > 1,
                0 is N mod 2,   % even
                Next is N//2,
                collatz(Next,S1),
                S is 1+S1.

collatz(N, S) :-
                N > 1,
                1 is N mod 2,   % odd
                Next is N*3 + 1,
                collatz(Next,S1),
                S is 1+S1.


is_prime(X) :- is_prime_aux(X,2).

is_prime_aux(X, D) :- 
    D >= X.


is_prime_aux(X,D) :-
                    X > D,
                    X mod D =\= 0,
                    D1 is D+1,
                    is_prime_aux(X,D1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tail Recursion %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


factorial_tail(N,F) :- factorial_tail_aux(N,F,1).

factorial_tail_aux(0,F,F).
factorial_tail_aux(N,F,Acc) :- N > 0,
                               N1 is N-1,
                               Acc1 is Acc * N,
                               factorial_tail_aux(N1,F,Acc1).






gcd(X, 0, G) :- G is X.

gcd(X, Y, G) :- Y =\= 0,
                X1 is X mod Y,
                gcd(Y,X1,G).



lcm(X, Y, M) :-
                Mul is X*Y,
                gcd(X,Y,G),
                M is Mul//G.