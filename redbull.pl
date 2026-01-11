% --- Pilots ---
pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(maclean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

% --- Teams ---
team_of(lamb, breitling).
team_of(besenyei, redbull).
team_of(chambliss, redbull).
team_of(maclean, mediterranean_racing_team).
team_of(mangold, cobra).
team_of(jones, matador).
team_of(bonhomme, matador).

% --- Aircrafts ---
pilots_aircraft(lamb, mx2).
pilots_aircraft(besenyei, edge540).
pilots_aircraft(chambliss, edge540).
pilots_aircraft(maclean, edge540).
pilots_aircraft(mangold, edge540).
pilots_aircraft(jones, edge540).
pilots_aircraft(bonhomme, edge540).

% --- Circuits ---
circuit(istanbul).
circuit(budapest).
circuit(porto).

% --- Winners ---
won(jones, porto).
won(mangold, budapest).
won(mangold, istanbul).

% --- Gates per circuit ---
gates(istanbul, 9).
gates(budapest, 6).
gates(porto, 5).


most_gates(X) :-
    gates(X,NumX),
    \+ (gates(_,NumY),NumY > NumX).

least_gates(X) :-
    gates(X,NumX),
    \+ (gates(_,NumY), NumY < NumX).

gate_diff(X) :-
    most_gates(Highest),
    least_gates(Lowest),
    gates(Highest,H),
    gates(Lowest,L),
    X is H - L.

same_team(X, Y) :-
    pilot(X),pilot(Y),
    X \= Y,
    team_of(X,Team),
    team_of(Y,Team).

is_from_winning_team(P, C) :-
    won(Winner,C),
    same_team(Winner,P).