
max(A, B, C, A) :- A >= B , A >= C, !.

max(A, B, C, B) :- B >= A, B >= C, !.

max(A, B, C, C).



print_n(0,_ ).

print_n(N, S) :-
                put_char(S),
                N1 is N-1,
                print_n(N1,S).
                

print_text(Text, Symbol, Padding) :-
                                    put_char(Symbol),
                                    !,
                                    print_n(Padding,' '),
                                    !,
                                    format('~s',[Text]),
                                    !,
                                    print_n(Padding,' '),
                                    put_char(Symbol).


print_banner(Text, Symbol, Padding) :-
                                        length(Text, LT),
                                        NumSym is LT + (Padding*2) + 2, %Padding appears two times
                                        print_n(NumSym,Symbol),
                                        nl, !,
                                        NumSpaces is NumSym -2,
                                        put_char(Symbol),
                                        print_n(NumSpaces,' '),
                                        put_char(Symbol),
                                        nl, !,
                                        print_text(Text,Symbol,Padding),
                                        nl, !,
                                        put_char(Symbol),
                                        print_n(NumSpaces,' '),
                                        put_char(Symbol),
                                        nl, !,
                                        print_n(NumSym,Symbol).


read_number(X) :- read_number_aux(X,0).
                

read_number_aux(X,Acc) :-
                        peek_code(10),
                        !,  % no going back after seeing the new line
                        get_code(_),
                        X = Acc.
                        
read_number_aux(X,Acc) :- 
                        get_code(Code),
                        Digit is Code - 48,              
                        NewAcc is Acc*10 + Digit,    
                        read_number_aux(X,NewAcc).                             
                                    

read_until_between(Min, Max, Value) :-
                                    repeat,
                                    read_number(N),
                                    N >= Min,
                                    N =< Max,
                                    !,
                                    Value = N.


read_string(X) :- 
    get_code(C),
    read_string_aux(C,X).

read_string_aux(10,[]) :- !. % New line so we stop reading

read_string_aux(Code,[Code|Result]) :-
                                    get_code(NextCode), % Get next code
                                    read_string_aux(NextCode,Result).


banner :- 
        write('Put here the text you want to create a banner from: '),
        read_string(Text),
        nl,
        write('Now the symbol: '),
        get_char(Symbol),
        skip_line,
        nl,
        write('Finally how much padding do you want: '),
        read_number(Padding),
        nl,
        print_banner(Text,Symbol,Padding).


print_full_list(L) :- put_char('['),
                      print_full_list_aux(L),
                      !,
                      put_char(']').

print_full_list_aux([H]) :- put_char(H), !.


print_full_list_aux([H|T]) :-
                        put_char(H),
                        put_char(','),
                        put_char(' '),
                        print_full_list_aux(T).


print_list(L) :-
                length(L,Len),
                Len =< 11, !,
                print_full_list(L).

print_list(L) :-
                append(First3,_,L),
                length(First3,3),

                append(_,Last3,L),
                length(Last3,3),
                !,
                length(L,Len),
                Skip is (Len-3)//2,
                append(Rejected,Rest,L),
                length(Rejected,Skip),
                append(Middle3,_,Rest),
                length(Middle3,3),

                put_char('['),
                print_full_list_aux(First3),
                write(', ...,'),
                print_full_list_aux(Middle3),
                write(', ...,'),
                print_full_list_aux(Last3),
                put_char(']').

print_matrix([]) :- !.

print_matrix([X|XS]) :-
                    print_full_list(X),
                    nl,
                    print_matrix(XS).


                