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




get_company_nodes(Company,ListOfNodes) :-
                        setof(Airport, (flight(Airport,_,Company,_,_,_); flight(_,Airport,Company,_,_,_)), ListOfNodes).

most_diversified(Company) :-
                        setof(Comp, flight(_,_,Comp,_,_,_) , Companies),
                        most_diversified_aux(Companies,ListCount),
                        sort(ListCount, ListSorted),
                        last(ListSorted,Company-_),
                        member(MaxCount-Company, ListSorted).
                        

most_diversified_aux([],[]).
most_diversified_aux([Company|T],[Len-Company|Rest]) :-
                        get_company_nodes(Company,List),
                        length(List, Len),
                        most_diversified_aux(T,Rest).
                        
find_flights(Origin, Destination, Flights) :-
                        dfs_flights(Origin,Destination,[Origin],Flights).

dfs_flights(F,F,_,[]).

dfs_flights(Current,Destination, Visited, [Code|Result]) :-
                        flight(Current,Neightbour,Code,_,_,_),
                        not(memberchk(Neightbour, Visited)),
                        dfs_flights(Neightbour,Destination,[Neightbour|Visited],Result).




find_flights_bfs(Origin, Destination, Flights) :-
    % A fila começa com o par: Origin - CaminhoVazio
    bfs_flights([Origin-[]], Destination, [Origin], Flights).

% 1. Caso Base: O primeiro elemento da fila é o Destino
bfs_flights([Destination-PathRev|_], Destination, _, Flights) :-
    reverse(PathRev, Flights). % O caminho foi construído ao contrário, inverte-se no fim

% 2. Passo Recursivo: O primeiro elemento NÃO é o destino
bfs_flights([Current-Path|RestQueue], Destination, Visited, Flights) :-
    findall(
        Next-[Code|Path],  % O que queremos na fila: Próximo nó e o caminho atualizado
        (
            flight(Current, Next, _, Code, _, _), % Encontra voo
            \+ member(Next, Visited),             % Verifica se já foi visitado globalmente
            \+ member_queue(Next, RestQueue)      % (Opcional) Verifica se já está na fila de espera
        ),
        NewNodes % Lista de novos pares Vizinho-Caminho
    ),
    
    % Atualiza visitados (apenas para a lógica não repetir nós)
    extract_nodes(NewNodes, NodesOnly),
    append(Visited, NodesOnly, NewVisited),
    
    % A Lógica BFS: Adiciona os novos no FIM da fila
    append(RestQueue, NewNodes, NewQueue),
    
    % Chama recursivamente
    bfs_flights(NewQueue, Destination, NewVisited, Flights).

% Auxiliares
% Verifica se um nó existe dentro da estrutura complexa da fila
member_queue(Node, Queue) :- member(Node-_, Queue).

% Extrai apenas os nomes dos aeroportos para atualizar a lista de visitados
extract_nodes([], []).
extract_nodes([Node-_|T], [Node|R]) :- extract_nodes(T, R).
                        
                        


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
                dfs_flights(Neightbour,Origin,[Origin,Neightbour],Flights),
                length([Code|Flights],L),
                MaxSize >= L,
                Cycle = [Code|Flights].

strongly_connected(ListOfNodes) :-
                strongly_connected_main(ListOfNodes,ListOfNodes).

strongly_connected_main([],_).
strongly_connected_main([H|T],List) :-
                can_reach(H,List),              % see if a node can reach all the others
                strongly_connected_main(T,List). % check if the next nodes can also reach all the other nodes

    
can_reach(Origin,[]).
can_reach(Origin,[H|T]) :-
                (Origin==H;dfs_flights(Origin,H,_)),
                can_reach(Origin,T).

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
