job(technician, eleuterio). 
job(technician, juvenaldo). 
job(analyst, leonilde). 
job(analyst, marciliano). 
job(engineer, osvaldo). 
job(engineer, porfirio). 
job(engineer, reginaldo). 
job(supervisor, sisnando). 
job(chief_supervisor, gertrudes). 
job(secretary, felismina). 
job(director, asdrubal). 
supervised_by(technician, engineer). 
supervised_by(engineer, supervisor). 
supervised_by(analyst, supervisor). 
supervised_by(supervisor, chief_supervisor). 
supervised_by(chief_supervisor, director). 
supervised_by(secretary, director).

% X a direct supervisor of Y
direct_supervisor(X,Y) :-
        job(JobX,X),
        job(JobY,Y),
        supervised_by(JobY,JobX).

supervised_by_same(X,Y) :-
        job(JobX,X),
        job(JobY,Y),
        supervised_by(JobX,_J),
        supervised_by(JobY,_J).

% X occupies position above Y
superior(X, Y) :-
        job(JobX,X),
        job(JobY,Y),
        superior_aux(JobY,JobX).

superior_aux(JobHigher,JobHigher).

%will trace JobY to JobX, by checking each superior until it reaches
% if it cannot find it then Job X is not superior
superior_aux(JobLower,JobHigher) :-
        supervised_by(JobLower,Job),
        superior_aux(Job,JobHigher).