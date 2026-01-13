:- use_module(library(lists)).

%% ==============================================================================
%%                            LÓGICA DO TABULEIRO (REGRAS)
%% ==============================================================================

%% ------------------------------------------------------------------------------
%% Restrição: next_to(X, Y, Board)
%% Objetivo: Verificar se a cor X está imediatamente ao lado da cor Y.
%% ------------------------------------------------------------------------------

% 1. Caso base (Trivial):
% Se a restrição pedir para X estar ao lado de X (ex: next_to(black, black)),
% assumimos que é verdade ou ignoramos, pois peças iguais não existem no jogo real,
% mas o predicado aceita para evitar falhas de lógica.
next_to(X,X,_). 

% 2. Caso Direto:
% Verifica se X aparece antes de Y na lista (Ex: [... X, Y ...]).
next_to(X,Y,[A,B,C,D,E,F]) :- 
    consecutive(X,Y,[A,B,C,D,E,F]). 

% 3. Caso Simétrico:
% A relação "ao lado" é comutativa. Se X está ao lado de Y, Y está ao lado de X.
% Verifica se Y aparece antes de X na lista (Ex: [... Y, X ...]).
next_to(X,Y,[A,B,C,D,E,F]) :- 
    consecutive(Y,X,[A,B,C,D,E,F]). 

%% Auxiliar: consecutive(X, Y, Board)
%% Verifica se dois elementos aparecem pegados numa lista.
consecutive(X,Y,Board) :-  
    % append/3 decompõe a lista em 3 partes:
    % Prefix: Coisas que estão antes (não nos interessam, por isso _)
    % [X,Y | Suffix]: Encontramos X seguido imediatamente de Y, e depois o resto.
    append(_Prefix, [X,Y|_Suffix], Board). 

%% ------------------------------------------------------------------------------
%% Restrição: anywhere(X, Board)
%% Objetivo: X pode estar em qualquer lugar.
%% ------------------------------------------------------------------------------
anywhere(X, Board) :-
    % member/2 verifica se X é um elemento da lista Board.
    % Como o tabuleiro final tem sempre as 6 cores, isto é sempre verdade
    % desde que X seja uma cor válida.
    member(X,Board).

%% ------------------------------------------------------------------------------
%% Restrição: one_space(X, Y, Board)
%% Objetivo: Verificar se existe exatamente UM lugar entre X e Y.
%% Exemplo: A e C (o B está no meio).
%% ------------------------------------------------------------------------------
one_space(X, Y, Board) :-
    (
        % Opção 1: X vem primeiro, depois algo (_), depois Y.
        % O '_' no meio representa "qualquer cor".
        append(_Prefix, [X, _, Y | _Suffix], Board)
        ; % O ponto e vírgula (;) significa "OU"
        % Opção 2 (Simetria): Y vem primeiro, depois algo (_), depois X.
        append(_Prefix, [Y, _, X | _Suffix], Board)
    ).

%% ------------------------------------------------------------------------------
%% Restrição: across(X, Y, Board)
%% Objetivo: Verificar se X está "em frente" a Y.
%% Estrutura do Tabuleiro:
%%   Fundo (Bottom): Posições 1 e 2 (A, B)
%%   Meio (Cabeceira): Posição 3 (C) - Não tem "frente" nestas regras.
%%   Topo (Upper): Posições 4, 5 e 6 (D, E, F)
%% Regra: A e B estão em frente a D, E e F.
%% ------------------------------------------------------------------------------
across(X, Y, [A,B,C,D,E,F]) :-
    Bottom = [A,B],      % Definimos a aresta de baixo (excluindo C)
    Upper = [D,E,F],     % Definimos a aresta de cima
    (
        % Caso 1: X está em baixo E Y está em cima
        (member(X,Bottom), member(Y,Upper))
        ; % OU
        % Caso 2: Y está em baixo E X está em cima
        (member(Y,Bottom), member(X,Upper))
    ).

%% ------------------------------------------------------------------------------
%% Restrição: same_edge(X, Y, Board)
%% Objetivo: Verificar se X e Y estão na MESMA aresta.
%% As arestas são definidas no enunciado como {A, B} e {D, E, F}.
%% A posição C não pertence a nenhuma aresta.
%% ------------------------------------------------------------------------------
same_edge(X, Y, [A,B,C,D,E,F]) :-
    Bottom = [A,B],      % Aresta 1
    Upper = [D,E,F],     % Aresta 2
    (
        % Caso 1: Ambos (X e Y) estão na aresta de baixo
        (member(X,Bottom), member(Y,Bottom))
        ; % OU
        % Caso 2: Ambos (X e Y) estão na aresta de cima
        (member(Y,Upper), member(X,Upper))
    ).

%% ------------------------------------------------------------------------------
%% Restrição: position(X, PositionsList, Board)
%% Objetivo: Verificar se a cor X está numa das posições permitidas (1 a 6).
%% ------------------------------------------------------------------------------

% Caso Base (Recursão):
% Tenta verificar se X está na posição indicada pela cabeça da lista [Pos].
position(X, [Pos|_RestPositions], Board) :-
    % nth1(Index, List, Element) verifica se o elemento no índice Index é Element.
    nth1(Pos, Board, X).

% Passo Recursivo:
% Se X não estava na posição 'Pos', ignoramos 'Pos' e tentamos o resto da lista.
position(X, [_|RestPositions], Board) :-
    position(X, RestPositions, Board).

%% ==============================================================================
%%                            PART 1: RESOLVER O PUZZLE
%% ==============================================================================

%% solve(+Constraints, -Board)
%% Argumentos:
%%   Constraints: Lista de restrições (ex: [next_to(green,blue)...])
%%   Board: A solução final (lista de 6 cores).
%% ------------------------------------------------------------------------------
solve(Constraints, Board) :-
    % 1. Estrutura: Define que a lista 'List' tem de ter tamanho 6.
    %    Neste momento 'List' é [_, _, _, _, _, _].
    length(List, 6),
    % 2. Aplicação de Restrições (Constraint Propagation):
    %    Passamos a lista vazia para 'call_constraints'. O Prolog tenta aplicar
    %    o que sabe sobre as variáveis.
    call_constraints(Constraints, List), 
    % 3. Definição do Domínio: Estas são as únicas cores válidas.
    ValidBoard = [green, yellow, blue, orange, white, black],
    % 4. Geração (Backtracking):
    %    permutation/2 tenta preencher 'List' com as cores de 'ValidBoard'.
    %    Se a permutação gerada violar as restrições definidas antes,
    %    o Prolog recua (backtracks) e tenta outra combinação.
    permutation(List, ValidBoard),
    % 5. Unificação Final: Se chegámos aqui, 'List' é uma solução válida.
    Board = List.
    

%% call_constraints(+ListConstraints, +Board)
%% Auxiliar que percorre a lista de restrições e aplica 'call' a cada uma.
call_constraints([], _Board). % Caso base: lista vazia, termina com sucesso.
call_constraints([Constraint | RestConstraints], Board) :-
    % 'call' executa o predicado guardado na variável Constraint, passando Board como argumento.
    % Ex: Se Constraint é next_to(green,blue), executa next_to(green,blue,Board).
    call(Constraint, Board),
    % Chama recursivamente para as restantes restrições.
    call_constraints(RestConstraints, Board).



%% ==============================================================================
%%                            PART 2: MELHOR PONTUAÇÃO
%% ==============================================================================

%% best_score(+Constraints, -Score)
%% Objetivo: Calcular a pontuação máxima possível para um conjunto de restrições,
%% sabendo que nem sempre é possível cumprir todas.
%% ------------------------------------------------------------------------------
best_score(Constraints, Score) :-
    ValidBoard = [green, yellow, blue, orange, white, black],
    
    % 1. Findall (Recolher todas as pontuações):
    %    O findall vai experimentar TODAS as permutações possíveis do tabuleiro.
    %    Para cada tabuleiro, executa 'calculate_score'.
    %    O resultado (S) é guardado na lista 'AllScores'.
    findall(S, (
        permutation(ValidBoard, Board), 
        calculate_score(Constraints, Board, 0, S)
    ), AllScores),
    
    % 2. Ordenação:
    %    sort/2 ordena a lista de pontuações do menor para o maior (ex: [-5, -2, -1, 0]).
    sort(AllScores, SortedScores),
    
    % 3. Inversão:
    %    reverse/2 inverte a lista para ter o maior valor à cabeça (ex: [0, -1, -2, -5]).
    reverse(SortedScores, ReversedScores),
    
    % 4. Extração:
    %    Unifica a variável Score com a cabeça da lista (o melhor valor).
    ReversedScores = [Score|_].
    
    
%% calculate_score(+Constraints, +Board, +CurrentScore, -FinalScore)
%% Calcula a pontuação de um tabuleiro específico.
%% Regra: Começa com 0. Cada restrição falhada subtrai 1 ponto.
%% ------------------------------------------------------------------------------

% Caso Base:
% Se não há mais restrições na lista ([]), o Score Final é igual ao acumulado (CurrentScore).
calculate_score([], _Board, Score, Score).

% Caso 1: A restrição é CUMPRIDA.
calculate_score([Constraint | RestConstraints], Board, CurrentScore, FinalScore) :-
    call(Constraint, Board), % Tenta executar a restrição. Se for verdade...
    !,                       % O Cut (!) é fundamental aqui. Diz ao Prolog:
                             % "Se a restrição passou, não tentes a regra de baixo (falha)".
                             % Isto impede que contemos a mesma restrição como sucesso e falha.
    
    % Chamada recursiva mantendo o CurrentScore igual.
    calculate_score(RestConstraints, Board, CurrentScore, FinalScore).

% Caso 2: A restrição FALHOU.
% O Prolog só vem para esta regra se a de cima falhar (porque 'call' falhou).
calculate_score([_Constraint | RestConstraints], Board, CurrentScore, FinalScore) :-
    NewScore is CurrentScore - 1, % Penaliza o score (subtrai 1).
    % Chamada recursiva com o novo score penalizado.
    calculate_score(RestConstraints, Board, NewScore, FinalScore).







%%%%%%%%%%%% Exercicios de testes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Exercicio que usa uma logica de DFS, para encontrar todas as respostas, para um predicado (alternativa para findall)
actor_movies(Actor, Movies) :-
    movies_acted(Actor,[],Movies).


movies_acted(Actor,Visited,Movies) :-
    by(_,Movie,Actor),
    \+ (memberchk(Movie,Visited)),!,
    movies_acted(Actor,[Movie|Visited],Movies).

movies_acted(_,Visited,Visited).


%% Exercicio tipico de encontrar um maximo para alguma coisa usando findall
diverse_books(Books) :-
    findall(Num-Title,(book(Title,_,_,_,Genres),length(Genres,Num)),List),
    sort(List,Sorted),
    last(Sorted,Max-_),
    findall(T,member(Max-T,Sorted),Books).


%% exercicio de negação

most_expensive_dish(Dish, Price) :-
    dish(Dish,Price,_),
    \+ ((dish(Dish2,Price2,_), Price2 > Price)). % por negação


%% exemplo DFS ciclo

circle_size(Person, Size) :-
    gives_gift_to(Person,_,Receiver),
    dfs(Receiver,Person,[Receiver],Path),
    length(Path,Size).

dfs(D,D,Visited,Visited).

dfs(Current,Destination,Visited,Path) :-
    gives_gift_to(Current,_,Next),
    \+ (memberchk(Next,Visited)),!,
    dfs(Next,Destination,[Next|Visited],Path).



%% Exemplo BFS esqueleto

find_flights_bfs(Origin, Destination, Path) :-
    bfs([[Origin]],Destination,L),
    reverse(L,Path).

bfs([[Dest|Path]|_],Dest,[Dest|Path]).

bfs([[Current|Path]|Rest],Dest,Result) :-
    findall([Next,Current|Path],(flight(Current,Next,_,_,_,_), \+ (memberchk(Next,[Current|Path]))),NextNodes),
    append(Rest,NextNodes,UpdatedQ),
    bfs(UpdatedQ,Dest,Result).



%% Exercicio de I/O

transform(0,'.').
transform(1,'M').

print_generation(List) :-
    print_generation(List,8).

print_generation(List,8) :-
    write('|'),!,
    print_generation(List,0).

print_generation([],_).

print_generation([Bit|Rest],Counter) :-
    transform(Bit,Char),
    write(Char),
    NextCounter is Counter + 1,
    print_generation(Rest,NextCounter).



%% Exemplo de usar findall e between para gerar coisas repetidas

initialize(DecNumber, Bits, Padding, List) :-
    dec2bin(DecNumber,BinList,Bits),
    findall(0,between(1, Padding, _),ListZeros),
    append(ListZeros,BinList,Temp),
    append(Temp,ListZeros,List).


%% Meta Programming

filterArgs(Term, Indexes, NewTerm) :-
    Term =.. [Name|_],
    getargs(Indexes,Term,NewArgs),
    NewTerm =.. [Name|NewArgs].


getargs([],_,[]).
getargs([Index|Indexes],Term,[Arg|Result]) :-
    arg(Index,Term,Arg),
    getargs(Indexes,Term,Result).




%% Exercicio de append
consume_ingredient(IngredientStocks, Ingredient, Grams, NewIngredientStocks) :-
    append(Prefix,[Ingredient-Amount|Suffix],IngredientStocks),
    NewAmount is Amount - Grams,
    append(Prefix,[Ingredient-NewAmount|Suffix],NewIngredientStocks).



% exercicio de encontrar componentes

common_subgraphs(G1, G2, Subgraphs) :-
    common_edges(G1,G2,L),
    nodes_from_edges(L,Nodes),
    find_components(Nodes,L,Subgraphs).


nodes_from_edges(Edges, Nodes) :-
    setof(X, A^B^(member(A-B, Edges), (X=A ; X=B)), Nodes).

find_components([],_,[]).
find_components([Node|Rest],Edges,[Component|Components]) :-
    bfs([Node],Edges,[],Component),
    subtract(Rest,Component,Subtracted),
    find_components(Subtracted,Edges,Components).

% bfs(+Fila, +Arestas, +Visitados, -Resultado)

bfs([],_,Visited,Visited).

bfs([Node|RestQueue],Edges,Visited,Result) :-
    member(Node,Visited),!,
    bfs(RestQueue,Edges,Visited,Result).

bfs([Node|RestQueue],Edges,Visited,Result) :-
    nodes_neightbours(Node,Edges,Neightbours),
    append(RestQueue,Neightbours,UpdatedQueue),
    bfs(UpdatedQueue,Edges,[Node|Visited],Result).
    

nodes_neightbours(Node,Edges,Neightbours) :-
    findall(X, (member(Node-X,Edges);member(X-Node,Edges)),Neightbours).


%% exercicios de Operadores

:- op(600,xfx,wrote).
:- op(610,xfx,at).

Author wrote Title at Age :-
    book(Title, AuthorID, YearOfRelease, Pages, Genres),
    author(AuthorID, Author, YearOfBirth, CountryOfBirth),
    Age is YearOfRelease - YearOfBirth.





%%%%%%%%%%%%%%%%%%%%%%%%%% Cuts %%%%%%%%%%%%%%%%%%%%%%%%

most_similar(Book, C1, C2, C1):-
similarity(Book, C1, S1),
similarity(Book, C2, S2),
S1 >= S2, !.
most_similar(Book, C1, C2, C2):-
similarity(Book, C1, S1),
similarity(Book, C2, S2),
S2 >= S1.

%The cut is red, as its removal would affect the results produced by most_similar/4.




%% ==============================================================================
%%                           NOTAS TEÓRICAS E TRUQUES (CHEAT SHEET)
%% ==============================================================================

%% --- 1. FINDALL vs BAGOF vs SETOF ---------------------------------------------
%  findall(Template, Goal, List).
%    - Devolve lista VAZIA [] se não encontrar soluções (NUNCA falha).
%    - NÃO remove duplicados.
%    - NÃO ordena.
%
%  bagof(Template, Goal, List).
%    - FALHA (dá 'no') se não encontrar soluções.
%    - Devolve resultados separados para variáveis livres (se existirem).
%
%  setof(Template, Goal, List).
%    - FALHA se não encontrar soluções.
%    - ORDENA a lista.
%    - REMOVE duplicados (é um conjunto matemático).

%% --- 2. O PADRÃO "KEY-VALUE" PARA ORDENAR -------------------------------------
%  Se quiseres encontrar o "Melhor" ou "Maior", usa o par 'Valor-Objeto'.
%  O sort/2 do Prolog ordena pelo primeiro elemento do par.
%
%  Exemplo: Encontrar o livro com mais géneros.
%    1. findall(Num-Titulo, (book(Titulo, ... Gen), length(Gen, Num)), Lista).
%    2. sort(Lista, Sorted).      % Ordena por Num (crescente).
%    3. last(Sorted, MaxNum-Livro). % Pega no último (o maior).
%    4. member(MaxNum-OutroLivro, Sorted). % Para tratar empates.

%% --- 3. BFS vs DFS (Qual usar?) -----------------------------------------------
%  DFS (Depth-First Search - Pilha/Recursão):
%    - Vantagem: Fácil de escrever (o Prolog já faz isto nativamente).
%    - Desvantagem: Pode entrar em loops infinitos (precisa de verificação de ciclos).
%    - Desvantagem: NÃO garante o caminho mais curto.
%
%  BFS (Breadth-First Search - Fila):
%    - Vantagem: Garante o CAMINHO MAIS CURTO (ótimo).
%    - Desvantagem: Mais código. Exige append ao final da fila.
%    - Truque: A fila é uma lista de listas: [[NoAtual, Pai, Avo...], [OutroCaminho...]]

%% --- 4. O CUT (!) - VERDE vs VERMELHO -----------------------------------------
%  Green Cut: Melhora performance, não muda soluções.
%  Red Cut: Muda a lógica (elimina soluções).
%
%  Padrão "If-Then-Else" com Cut (usado no calculate_score):
%    regra(X) :- condicao(X), !, faz_algo.  % Se condição for verdade, PÁRA AQUI.
%    regra(X) :- faz_outra_coisa.           % Só executa se a de cima falhou.

%% --- 5. NEGAÇÃO (\+) ----------------------------------------------------------
%  \+ Goal  (ou 'not(Goal)')
%  - A negação em Prolog é "Negação por Falha".
%  - \+ X > 5  significa "Não consegui provar que X > 5".
%  - CUIDADO: \+ (member(X, L)) não atribui valor a X. Só verifica.
%    As variáveis dentro do \+ devem estar instanciadas antes de chamares a negação.

%% --- 6. META-PROGRAMAÇÃO (=.. e arg) ------------------------------------------
%  Term =.. List
%    Ex: pai(joao, maria) =.. [pai, joao, maria].
%    Útil para construir predicados dinamicamente ou trocar argumentos.
%
%  arg(N, Term, Value)
%    Ex: arg(2, data(10, 20, 30), X). -> X = 20.
%    Mais eficiente que =.. se só quiseres aceder a um valor específico.

%% --- 7. ARITMÉTICA (IS vs = vs =:=) -------------------------------------------
%  X is 5 + 3.    -> X fica 8 (Calcula e atribui).
%  X = 5 + 3.     -> X fica o termo '5+3' (Unificação estrutural).
%  8 =:= 5 + 3.   -> True (Compara valores numéricos, ambos os lados resolvidos).
%  X =:= 5 + 3.   -> Erro se X não tiver valor.

%% --- 8. MANIPULAÇÃO DE LISTAS (O Padrão Select) -------------------------------
%  Para retirar um elemento de uma lista e ficar com o resto:
%    select(Elem, Lista, Resto).
%  
%  Implementação manual com append (muito comum em testes):
%    append(Prefix, [Elem|Suffix], Lista), append(Prefix, Suffix, Resto).

