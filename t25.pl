
% gives_gift_to(Giver, Gift, Receiver)
gives_gift_to(bernardete, 'The Exchange', celestina).
gives_gift_to(celestina, 'The Brethren', eleuterio).
gives_gift_to(eleuterio, 'The Summons', felismina).
gives_gift_to(felismina, 'River God', juvenaldo).
gives_gift_to(juvenaldo, 'Seventh Scroll', leonilde).
gives_gift_to(leonilde, 'Sunbird', bernardete).
gives_gift_to(marciliano, 'Those in Peril', nivaldo).
gives_gift_to(nivaldo, 'Vicious Circle', sandrino).
gives_gift_to(sandrino, 'Predator', marciliano).


circle_size(Person, Size) :-
    gives_gift_to(Person,_,Receiver),
    dfs(Receiver,Person,[Receiver],Path),
    length(Path,Size).

dfs(D,D,Visited,Visited).

dfs(Current,Destination,Visited,Path) :-
    gives_gift_to(Current,_,Next),
    \+ (memberchk(Next,Visited)),!,
    dfs(Next,Destination,[Next|Visited],Path).



largest_circle(People) :-
    findall(Size-Circle,(gives_gift_to(Person,_,_),circle(Person,Size,Circle)),List),
    sort(List,Sorted), reverse(Sorted,Reversed),
    Reversed = [Max-_|_],
    findall(C,member(Max-C,Sorted),MaxCircles),
    member(People,MaxCircles).

circle(Person,Size,Path) :-
    gives_gift_to(Person,_,Receiver),
    dfs(Receiver,Person,[Receiver],L),
    sort(L,Path),   % so that each circle is equal when we start for any of its members
    length(Path,Size).


dec2bin(0,[],0) :- !.
dec2bin(Dec, [Bit|BinList], N) :-
    N > 0,
    Bit is Dec mod 2,
    NextDec is Dec//2,
    N1 is N - 1,
    dec2bin(NextDec,BinList,N1).


initialize(DecNumber, Bits, Padding, List) :-
    dec2bin(DecNumber,BinList,Bits),
    findall(0,between(1, Padding, _),ListZeros),
    append(ListZeros,BinList,Temp),
    append(Temp,ListZeros,List).


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
    

