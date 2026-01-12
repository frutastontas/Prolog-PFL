:- use_module(library(lists)).

% by(Character, Movie, Actor)
by(jackRyan, theSumOfAllFears, benAffleck).
by(cathyMuller, theSumOfAllFears, bridgetMoynahan).
by(jackRyan, theHuntForRedOctober, alecBaldwin).
by(jackRyan, patriotGames, harrisonFord).
by(cathyMuller, patriotGames, anneArcher).
by(jackRyan, clearAndPresentDanger, harrisonFord).
by(cathyMuller, clearAndPresentDanger, anneArcher).
by(president, airForceOne, harrisonFord).
by(frasierCrane, cheers, kelseyGrammer).
by(frasierCrane, frasier, kelseyGrammer).
by(rachelGreen, friends, jenniferAniston).
by(monicaGeller, friends, courteneyCox).
by(phoebeBuffay, friends, lisaKudrow).
by(ursulaBuffay, friends, lisaKudrow).
by(joeyTribbiani, friends, mattLeBlanc).
by(joeyTribbiani, joey, mattLeBlanc).
by(alexGarrett, joey, andreaAnders).
by(stephenColbert, dailyShow, stephenColbert).
by(stephenColbert, theColbertReport, stephenColbert).
by(addisonMontgomery, privatePractice, kateWalsh).
by(addisonMontgomery, greysAnatomy, kateWalsh).
by(mattMurdock, daredevil, benAffleck).
by(elektraNatchios, daredevil, jenniferGarner).
by(elektraNatchios, elektra, jenniferGarner).
by(elektraNatchios, elektra, lauraWard).
by(sydneyBristow, alias, jenniferGarner).


plays_twins(Actor, Movie) :-
    by(Character1,Movie,Actor),
    by(Character2,Movie,Actor),
    Character1 \= Character2.

actor_movies(Actor, Movies) :-
    movies_acted(Actor,[],Movies).


movies_acted(Actor,Visited,Movies) :-
    by(_,Movie,Actor),
    \+ (memberchk(Movie,Visited)),!,
    movies_acted(Actor,[Movie|Visited],Movies).

movies_acted(_,Visited,Visited).


playedBy(Character, List) :-
    findall(Actor-ListOfMovies,(by(Character,_,Actor), get_movies_char(Character,Actor,ListOfMovies)),L),
    sort(L,List).

get_movies_char(Character,Actor,List) :-
    findall(Movie,(by(Character,Movie,Actor)),List).


most_popular(Exclude, List, NMovies) :-
    findall(Number-Actor,(by(_,_,Actor), \+ (memberchk(Actor,Exclude)) ,number_movies(Actor,Number)),L),
    sort(L,Sorted),reverse(Sorted,Reversed),
    Reversed = [NMovies-_|_],
    findall(Actr,(member(NMovies-Actr,Sorted)),List).

number_movies(Actor,NMovies) :-
    findall(Movie,(by(_,Movie,Actor)),L),
    sort(L,List),
    length(List, NMovies).




connection_link(Actor1, Actor2, ConnectionList) :-
    dfs(Actor1,Actor2,[Actor1],ConnectionList).

dfs(Current,Destination,Visited,ConnectionList) :-
    by(_,Movie,Current),
    \+ (member(Movie,Visited)),
    by(_,Movie,Destination),
    List = [Destination,Movie|Visited],
    reverse(List,ConnectionList).

dfs(Current,Destination,Visited,ConnectionList) :-
    by(_,Movie,Current),
    \+ (member(Movie,Visited)),
    by(_,Movie,Next),
    Current \= Next,
    \+ (member(Next,Visited)),
    dfs(Next,Destination,[Next,Movie|Visited],ConnectionList).




connection_bfs(Actor1, Actor2, ConnectionList) :-
    bfs([[Actor1]],Actor2,ConnectionList).

bfs([[Destination|Path]|_],Destination,ConnectionList) :-
    reverse([Destination|Path],ConnectionList).

bfs([[Current|Path]|RestQ],Destination,ConnectionList) :-
    findall([Next,Movie,Current|Path],(by(_,Movie,Current),\+ (member(Movie,Path))
        ,by(_,Movie,Next),Current \= Next,\+ (member(Next,Path))),NewPaths),
    append(RestQ,NewPaths,UpdatedQ),
    bfs(UpdatedQ,Destination,ConnectionList).




pretty_print(ConnectionList) :-
    ConnectionList = [Actor1,Movie,Actor2|Rest],
    write(Actor1),write(' worked in '),write(Movie), write(' with '),write(Actor2),nl,
    process_rest(Rest).

process_rest([]).

process_rest([M,A|Rest]) :-
    write(' who worked in '), write(M), write(' with '), write(A), nl,
    process_rest(Rest).
