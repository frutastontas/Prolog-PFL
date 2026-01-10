%author(AuthorID, Name, YearOfBirth, CountryOfBirth).
author(1, 'John Grisham', 1955, 'USA').
author(2, 'Wilbur Smith', 1933, 'Zambia').
author(3, 'Stephen King', 1947, 'USA').
author(4, 'Michael Crichton', 1942, 'USA').
%book(Title, AuthorID, YearOfRelease, Pages, Genres).
book('The Firm', 1, 1991, 432, ['Legal thriller']).
book('The Client', 1, 1993, 422, ['Legal thriller']).
book('The Runaway Jury', 1, 1996, 414, ['Legal thriller']).
book('The Exchange', 1, 2023, 338, ['Legal thriller']).
book('Carrie', 3, 1974, 199, ['Horror']).
book('The Shining', 3, 1977, 447, ['Gothic novel', 'Horror', 'Psychological horror']).
book('Under the Dome', 3, 2009, 1074, ['Science fiction', 'Political']).
book('Doctor Sleep', 3, 2013, 531, ['Horror', 'Gothic', 'Dark fantasy']).
book('Jurassic Park', 4, 1990, 399, ['Science fiction']).
book('Prey', 4, 2002, 502, ['Science fiction', 'Techno-thriller', 'Horror',
'Nanopunk']).
book('Next', 4, 2006, 528, ['Science fiction', 'Techno-thriller', 'Satire']).



book_author(Title, Author) :-
    book(Title,AuthorID,_,_,_),
    author(AuthorID,Author,_,_).

multi_genre_book(Title) :-
    book(Title,_,_,_,Genres),
    length(Genres,Len),
    Len > 1.

shared_genres(Title1, Title2, CommonGenres) :-
    book(Title1, _,_,_, Genres1),
    book(Title2, _,_,_, Genres2),
    common_genres(Genres1,Genres2,CommonGenres).

common_genres([],_,[]).
common_genres([Genre|Rest],Genres2,[Genre|Genres]) :-   
    member(Genre,Genres2),!,
    common_genres(Rest,Genres2,Genres).

common_genres([_|Rest],Genres2,Genres) :- 
    common_genres(Rest,Genres2,Genres).


similarity(Title1, Title2, Similarity) :-
    shared_genres(Title1,Title2,CommonGenres),
    book(Title1, _,_,_, Genres1),
    book(Title2, _,_,_, Genres2),
    append(Genres1,Genres2,AllGenres),
    sort(AllGenres,Union),
    length(CommonGenres,C),
    length(Union,U),
    Similarity is C/U.


author_wrote_book_at_age(Author, Title, Age) :-
    author(AuthorID, Author, YearOfBirth, CountryOfBirth),
    book(Title, AuthorID, YearOfRelease, Pages, Genres),
    Age is YearOfRelease - YearOfBirth.


youngest_author(Author) :-
    author(AuthorID, Author, YearOfBirth, CountryOfBirth),
    book(Title, AuthorID, YearOfRelease, Pages, Genres),
    author_wrote_book_at_age(Author,Title,Age),
    \+ (author(AuthorID2, Author2, _, _),
        book(Title2, AuthorID2, _,_, _), 
        author_wrote_book_at_age(Author2,Title2,Age2),
        Age2 < Age
    ).


% This is a failure driven loop, make sure that when you write a Genre, you fail, to make prolog backtrack and write another
% Always put a safety net when the whole predicate fails
genres(Title) :-
    book(Title, AuthorID, YearOfRelease, Pages, Genres),
    member(Genre,Genres),
    write(Genre),nl,
    fail.

genres(_T).


filterArgs(Term, Indexes, NewTerm) :-
    Term =.. [Name|_],
    getargs(Indexes,Term,NewArgs),
    NewTerm =.. [Name|NewArgs].


getargs([],_,[]).
getargs([Index|Indexes],Term,[Arg|Result]) :-
    arg(Index,Term,Arg),
    getargs(Indexes,Term,Result).



diverse_books(Books) :-
    findall(Num-Title,(book(Title,_,_,_,Genres),length(Genres,Num)),List),
    sort(List,Sorted),
    last(Sorted,Max-_),
    findall(T,member(Max-T,Sorted),Books).



country_authors(Country, Authors) :-
    setof(Author,(AuthorID,YearOfBirth)^
    (author(AuthorID, Author, YearOfBirth, Country))
    ,Authors).


read_book(bernardete, 'The Firm').
read_book(bernardete, 'The Client').
read_book(clarice, 'The Firm').
read_book(clarice, 'Carrie').
read_book(deirdre, 'The Firm').
read_book(deirdre, 'Next').

popular(Title) :-
    get_all_members(Members),
    book(Title,_,_,_,_),
    findall(Person,read_book(Person,Title),ListReadBook),
    length(Members,N),
    length(ListReadBook,NumberRead),
    Ratio is NumberRead/N,
    Ratio >= 0.75.
    


get_all_members(Members) :-
    findall(Member,read_book(Member,_),List),
    sort(List,Members).


:- op(600,xfx,wrote).
:- op(590,xfx,at).

Author wrote Title at Age :-
    book(Title, AuthorID, YearOfRelease, Pages, Genres),
    author(AuthorID, Author, YearOfBirth, CountryOfBirth),
    Age is YearOfRelease - YearOfBirth.


rotate(List, Position, Rotations, NewList) :-
    append(Before,[ListToRotate|After],List),
    Index is Position -1,
    length(Before,Index),

    append(Prefix,Suffix,ListToRotate),
    length(Prefix,Rotations),

    append(Suffix,Prefix,Rotated),
    append(Before,[Rotated|After],NewList).

matches([],[]).
matches([List|Lists], [Code|Codes]) :-
    List = [Code|_],
    matches(Lists,Codes).


move(Initial, Final) :-
    length(Initial,Len),    % get the size of the dial
    nth1(1,Initial,List),
    length(List,Size), % get the size of each row
    MaxRotations is Size -1,
    between(1,Len, Index),  % for (i=)
    between(1,MaxRotations,Rotations) % for (j=)
    rotate(Initial,Index,Rotations,Final).
    

% ==============================================================================
% PREDICADO PRINCIPAL: solve/3
% ==============================================================================
% Inicializa a pesquisa.
% Code: Estado Inicial
% Key:  Estado Objetivo (ou chave para o encontrar)
% States: A lista final com a solução
solve(Code, Key, States) :-
    % Chama o BFS criando a estrutura de FILA inicial.
    % A Fila é uma lista de listas: [[Inicio]]
    bfs([[Code]], Key, States).


% ==============================================================================
% BFS - CASO DE PARAGEM (SUCESSO)
% ==============================================================================
% Ocorre quando o primeiro caminho da fila atinge o objetivo.
bfs([[Code|Path]|_], Key, States) :-
    matches(Code, Key),          % 1. Verifica se o nó atual (Code) é o objetivo
    !,                           % 2. CUT: Se encontrámos, paramos de procurar (Green Cut)
    reverse([Code|Path], States). % 3. Como construímos o caminho "para trás" (Do fim para o início),
                                  %    temos de inverter para dar a resposta correta.


% ==============================================================================
% BFS - PASSO RECURSIVO (EXPANSÃO)
% ==============================================================================
% Se o primeiro caminho da fila não é solução, expandimos os seus vizinhos.
bfs([[Code|Path]|Rest], Key, States) :-
    % 1. ENCONTRAR VIZINHOS (Next)
    %    - move(Code, Next): Gera um movimento válido
    %    - \+ member(...): Evita ciclos (não volta a um estado já visitado neste caminho)
    findall(Next, 
            (move(Code, Next), \+ member(Next, [Code|Path])), 
            NextCodes),
    
    % 2. CRIAR NOVOS CAMINHOS
    %    Pega nos vizinhos novos e "cola" o histórico antigo a cada um.
    %    Ex: Se NextCodes=[a,b] e OldPath=[start], cria [[a,start], [b,start]]
    add_old_path(NextCodes, [Code|Path], NewPaths),
    
    % 3. ATUALIZAR A FILA (O passo CRUCIAL do BFS)
    %    append(Rest, NewPaths, ...): Coloca os NOVOS caminhos no FIM da fila.
    %    Isto garante que visitamos camada por camada (FIFO).
    append(Rest, NewPaths, UpdatedQ),
    
    % 4. RECURSÃO com a nova fila
    bfs(UpdatedQ, Key, States).


% ==============================================================================
% AUXILIAR: add_old_path/3
% ==============================================================================
% Transforma uma lista de nós vizinhos numa lista de caminhos completos.

% Caso Base: Se não há mais vizinhos para adicionar, a lista de novos caminhos é vazia.
add_old_path([], _, []).

% Passo Recursivo:
% Pega no vizinho 'Next', cria o caminho [Next | OldPath] e continua para o resto.
add_old_path([Next|Rest], OldPath, [[Next|OldPath]|Result]) :-
    add_old_path(Rest, OldPath, Result).




