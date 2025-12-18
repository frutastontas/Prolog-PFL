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
    class(Course1, _, Day, _, _),  % Encontra o dia da primeira cadeira
    class(Course2, _, Day, _, _).  % Verifica se a segunda cadeira tem aula nesse MESMO dia


daily_courses(Day, Courses) :-
            findall(Course,class(Course,_,Day,_,_), Courses).


short_classes(L) :-
            findall(Course-Day/Time, (class(Course,_,Day,Time,Duration), Duration < 2), L).


schedule :-
        setof((DayOfWeek,Time,Course,ClassType,Duration),class(Course,ClassType,DayOfWeek,Time,Duration), List).

