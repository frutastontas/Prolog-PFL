% teaches predicate
teaches(algorithms,adalberto).
teaches(databases,bernardete).
teaches(compilers,capitolino).
teaches(statistics, diogenes).
teaches(networks, ermelinda).



%attends predicate
attends(algorithms, alberto).
attends(algorithms, bruna).
attends(algorithms, cristina).
attends(algorithms, diogo).
attends(algorithms, eduarda).

% Statistics
attends(statistics, antonio).
attends(statistics, bruno).
attends(statistics, cristina).
attends(statistics, duarte).
attends(statistics, eduardo).

% Databases
attends(databases, alberto).
attends(databases, bernardo).
attends(databases, clara).
attends(databases, diana).
attends(databases, eurico).

% Compilers
attends(compilers, antonio).
attends(compilers, bruna).
attends(compilers, claudio).
attends(compilers, duarte).
attends(compilers, eva).

% Networks
attends(networks, alvaro).
attends(networks, beatriz).
attends(networks, claudio).
attends(networks, diana).
attends(networks, eduardo).



isStudent(X,Y) :- attends(C,X) , teaches(C,Y).

students(X,Y) :- teaches(C,X) , attends(C,Y).

teachers(X,Y) :- attends(C,X) , teaches(C,Y).

bothStudent(X,Y,S) :- isStudent(S,X) , isStudent(S,Y).

colleagues(X, Y) :-
    X \= Y,
    (
        % Students are colleagues if they share a course
        (attends(C, X), attends(C, Y));

        % Teachers are colleagues
        (teaches(_, X), teaches(_, Y))
    ).

moreThanOne(X) :- attends(Course1,X) ,
                  attends(Course2,X) ,
                  Course1 \= Course2 .

