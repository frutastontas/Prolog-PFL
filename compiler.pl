code(1).
code(2).
code(3).

meaning(1,'Integer Overflow').
meaning(2,'Division by zero').
meaning(3,'ID Unknown').

translate(X,Y) :- meaning(X,Y).

