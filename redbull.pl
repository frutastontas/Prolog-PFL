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

% --- Rule: a team wins if one of its pilots wins ---
team_wins(Team, Race) :-
    team_of(Pilot, Team),
    won(Pilot, Race).



