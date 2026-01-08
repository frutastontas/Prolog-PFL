%flight(Origin, Destination, Company, Code, Hour, Duration) 
flight(porto, lisbon, tap, tp1949, 1615, 60). 
flight(lisbon, madrid, tap, tp1018, 1805, 75). 
flight(lisbon, paris, tap, tp440, 1810, 150). 
flight(lisbon, london, tap, tp1366, 1955, 165). 
flight(london, lisbon, tap, tp1361, 1630, 160). 
flight(porto, madrid, iberia, ib3095, 1640, 80). 
flight(madrid, porto, iberia, ib3094, 1545, 80). 
flight(madrid, lisbon, iberia, ib3106, 1945, 80). 
flight(madrid, paris, iberia, ib3444, 1640, 125). 
flight(madrid, london, iberia, ib3166, 1550, 145). 
flight(london, madrid, iberia, ib3163, 1030, 140). 
flight(porto, frankfurt, lufthansa, lh1177, 1230, 165). 


airport(X) :- flight(X, _, _, _, _, _).
airport(X) :- flight(_, X, _, _, _, _).

get_all_nodes(ListOfAirports) :-
    setof(Airport, airport(Airport), ListOfAirports).

company(X) :- flight(_, _, X, _, _, _).

company_flights(Company,List) :-
    setof(Destination,Origin^Code^Hour^Duration^(flight(Origin,Destination,Company,Code,Hour,Duration)),List).

most_diversified(Company) :-
    setof(N-Comp,List^(company(Comp),company_flights(Comp,List),length(List,N)),ListOfCompanies),
    last(ListOfCompanies,_-Company).


find_flights(Origin, Destination, Flights) :-
    find_flights(Origin,Destination,[Origin],Flights).

find_flights(D,D,_,[]).
find_flights(Origin,Destination,Visited,[Code|Result]) :-
    flight(Origin,Next,_,Code,_,_),
    not(memberchk(Code,Visited)),
    find_flights(Next,Destination,[Next|Visited],Result).


find_all_flights (Origin, Destination, ListOfFlights) :-
    findall(ListOfCodes,
         find_flights(Origin,Destination,ListOfCodes)
         ,ListOfFlights).

find_flights_least_stops(Origin, Destination, ListOfFlights) :-
    setof(N-ListOfCodes,
         (find_flights(Origin,Destination,ListOfCodes),length(ListOfCodes,N))
         ,List),
    List = [Min-_|_],
    findall(L,member(Min-L,List),ListOfFlights).

find_flights_stops(Origin, Destination, Stops, ListFlights) :-
    findall(L,dfs_with_stops(Origin,Destination,Stops,L),ListFlights).

dfs_with_stops(Current,D,[],List):-
    find_flights(Current,D,List).
dfs_with_stops(Origin,Destination,[Stop|Stops],List) :-
    find_flights(Origin,Stop,ListStop),
    dfs_with_stops(Stop,Destination,Stops,Rest),
    append(ListStop,Rest,List).

find_circular_trip (MaxSize, Origin, Cycle) :-
    flight(Origin,Next,_,Code,_,_),
    find_flights(Next,Origin,[Next],List),
    append([Code],List,Cycle),
    length(Cycle,Len),
    MaxSize >= Len.

