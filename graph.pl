%flight(origin, destination, company, code, hour, duration) 
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

get_all_nodes(ListOfAirports) :-
                            setof(Airport, (flight(Airport,_,_,_,_,_); flight(_,Airport,_,_,_,_)), ListOfAirports).




most_diversified(Company) :-
    findall(Len-Comp,(flight(_,_,Comp,_,_,_),get_company_destinations(Comp,ListOfDestinations),length(ListOfDestinations,Len)),L),
    sort(L,Sorted),reverse(Sorted,Reversed),
    Reversed = [Max-_|_],
    findall(C,member(Max-C,Sorted),ListOfCompanies),
    member(Company,ListOfCompanies).


get_company_destinations(Company,ListOfDestinations) :-
    findall(Place,(flight(Place,_,Company,_,_,_);flight(_,Place,Company,_,_,_)),L),
    sort(L,ListOfDestinations).








                        
find_flights(Origin, Destination, Flights) :-
                        dfs_flights(Origin,Destination,[Origin],Flights).

dfs_flights(F,F,_,[]).

dfs_flights(Current,Destination, Visited, [Code|Result]) :-
                        flight(Current,Neightbour,Code,_,_,_),
                        not(memberchk(Neightbour, Visited)),
                        dfs_flights(Neightbour,Destination,[Neightbour|Visited],Result).



%%%% Exercise can be useful to put in the file for the test
find_flights_bfs(Origin, Destination, Path) :-
    bfs([[Origin]],Destination,L),
    reverse(L,Path).

bfs([[Dest|Path]|_],Dest,[Dest|Path]).

bfs([[Current|Path]|Rest],Dest,Result) :-
    findall([Next,Current|Path],(flight(Current,Next,_,_,_,_), \+ (memberchk(Next,[Current|Path]))),NextNodes),
    append(Rest,NextNodes,UpdatedQ),
    bfs(UpdatedQ,Dest,Result).
                        
                        


find_all_flights (Origin, Destination, ListOfFlights) :-
                findall(Flights, find_flights(Origin,Destination,Flights) , ListOfFlights)


find_flights_least_stops(Origin, Destination, ListOfFlights) :-
                setof(L-Flights, (find_flights(Origin,Destination,Flights),length(Flights,L)) , Sorted),

                Sorted = [Min-_|_],

                findall(Path, member(Min-Path, Sorted), ListOfFlights).





find_flights_stops(Origin, Destination, Stops, ListFlights) :-
                findall(Flights , dfs_with_stops(Origin,Destination,Stops,[],Flights), ListFlights).


dfs_with_stops(Current,Destination,[],Acc,Flights) :-
                find_flights(Current,Destination,NextFlights),
                append(Acc,NextFlights,Flights).
                

dfs_with_stops(Current,Destination,[S|R],Acc,Flights) :-
                find_flights(Current,S,NextFlights),
                append(Acc,NextFlights,UpdatedAcc),
                dfs_with_stops(S,Destination,R,UpdatedAcc,Flights).


find_circular_trip(MaxSize, Origin, Cycle) :-
                flight(Origin,Neightbour,Code,_,_,_),
                dfs_flights(Neightbour,Origin,[Neightbour],Flights),
                length([Code|Flights],L),
                MaxSize >= L,
                Cycle = [Code|Flights].




strongly_connected(ListOfNodes) :-
    strongly_connected(ListOfNodes,ListOfNodes).

strongly_connected([],_).
strongly_connected([H|T],List) :-
    can_reach(H,List),
    strongly_connected(T,List).

can_reach(_,[]).

can_reach(Origin,[Dest|Rest]) :-
    (Origin=Dest;find_flights(Origin,Dest,_)),
    can_reach(Origin,Rest).







strongly_connected_components(Components) :-
    get_all_nodes(AllNodes),             % 1. Vai buscar todos (já tens este predicado)
    find_components(AllNodes, Components). % 2. Chama o loop recursivo


find_components([],[]).
find_components([Node|Rest],[Scc|Result]) :-
        build_Scc(Node,[Node|Rest],Scc),

        subtract(Rest,Scc,UpdatedList),

        find_components(UpdatedList,Result).
        

build_Scc(Node,[],[]).
build_Scc(Node, [H|T], Scc) :-
        dfs_flights(Node,H,_),
        dfs_flights(H,Node,_),

        build_Scc(Node,T,RestScc),
        Scc = [H|RestScc].

build_Scc(Node,[H|T],Scc) :-
        build_Scc(Node,T,Scc).

unifiable([],_,[]).

unifiable([H|T], Term, L2) :-
            not(H = Term), % se isto der match significa que nao sao unifiables, nao adicionamos a lista
            unifiable(T,Term,L2).

unifiable([H|T],Term,[H|L2]) :-
            unifiable(T,Term,L2).


op(700, fy, flight).
op(600,xfy,from).
op(500,xfy,to).
op(400,xfy,at).
op(300,yfy,:).

op(700,fy,if).
op(600,xfy,then).
op(700,yfx,else).


if X then Y else Z :-
        call(X),
        !,
        call(Y).

if X then Y else Z :-
        call(Z).