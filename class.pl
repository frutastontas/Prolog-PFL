%class(Course, ClassType, DayOfWeek, Time, Duration) 
class(pfl, t, '2 Tue', 15, 2). 
class(pfl, tp, '2 Tue', 10.5, 2). 
class(lbaw, t, '3 Wed', 10.5, 2). 
class(lbaw, tp, '3 Wed', 8.5, 2). 
class(ipc, t, '4 Thu', 14.5, 1.5). 
class(ipc, tp, '4 Thu', 16, 1.5). 
class(fsi, t, '1 Mon', 10.5, 2). 
class(fsi, tp, '5 Fri', 8.5, 2). 
class(rc, t, '5 Fri', 10.5, 2). 
class(rc, tp, '1 Mon', 8.5, 2).


same_day(Course1, Course2) :-
    class(Course1,_,Day,_,_),
    class(Course2,_,Day,_,_).

daily_courses(Day, Courses) :-
    findall(Course, class(Course,_,Day,_,_), Courses).


short_classes(L) :-
    setof(Course-DayOfWeek/Time,ClassType^Duration^(class(Course,ClassType,DayOfWeek,Time,Duration),Duration < 2),L).    

course_classes(Course, Classes) :-
    findall(DayOfWeek/Time-ClassType,class(Course,ClassType,DayOfWeek,Time,_),Classes).

courses(L) :-
    setof(Course,ClassType^DayOfWeek^Time^Duration^(class(Course,ClassType,DayOfWeek,Time,Duration)),L).


schedule :-
    setof(DayOfWeek-Course-ClassType,Time^Duration^(class(Course,ClassType,DayOfWeek,Time,Duration)),List),
    display_class(List).

display_class([]).
display_class([DayOfWeek-Course-ClassType|Rest]) :-
    format('Course-~w, Type-~w, Day-~w ~n',[Course,ClassType,DayOfWeek]),
    display_class(Rest).


find_class :-
    write('Give us a day: '),
    read(_X),nl,
    write('Give us a time: '),
    read(_Y),nl,
    setof(Course-Time-Duration,ClassType^(class(Course,ClassType,_X,Time,Duration)),List),
    taking_place(List,_Y,Result),
    write(Result).

taking_place([],_,[]).

taking_place([Course-Time-Duration|Rest],GivenTime,[Course-Time-Duration|Result]) :-
    Time =< GivenTime,
    EndTime is Time + Duration,
    EndTime > GivenTime, !,
    taking_place(Rest,GivenTime,Result).

taking_place([_|Rest],GivenTime,Result) :-
    taking_place(Rest,GivenTime,Result).
