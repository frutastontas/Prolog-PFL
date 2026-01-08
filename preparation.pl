
list_shift_rotate(List1, N, List2) :-
    append(Rotate,Rest,List1),
    length(Rotate, N),!,
    append(Rest,Rotate,List2).

    
rle([], []).

rle([H|T],[H-N|Rest]) :-
    group(==H,[H|T],Front,Back),
    length(Front,N),
    rle(Back,Rest).


pascal(1,[[1]]) :- !.
pascal(N, Lines) :-
    N1 is N-1,
    pascal(N1,LastLines),
    last(LastLines,LastLine),
    generate_next_row(LastLine,New),
    append([1],New,NewLine),
    append(LastLines,[NewLine],Lines).


generate_next_row([_],[1]).
generate_next_row([A,B|Rest],[Sum|NewRow]) :-
    Sum is A+B,
    generate_next_row([B|Rest],NewRow).


