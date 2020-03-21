
insert(E,L,[E|L]).
insert(E,[H|T],[H|A]):-
	insert(E,T,A).
	
perm([],[]).
perm([H|T],A):-
	perm(T,A1),
	insert(H,A1,A).
	
get_subsets([],[]).

get_subsets([H|T],[H|L2]):-
	\+ H = activity(_),
    get_subsets(T,L2).
    
get_subsets([activity(X)|T],[activity(H1)|T1]):-
	
	get_subsets(X,H1),
	H1 \= [],
	get_subsets(T,T1).
	
get_subsets([_|T],L):-
    get_subsets(T,L).
	
possibleSubset(L,A):-
	get_subsets(L,A1),
	perm(A1,A).
	
choosePreferences(L,R):-
	get_subsets(L,R).
	
bigger_Or_Equal_Period(A,B,C,X,Y,Z):-
    A>X;(A==X,B>Y);(A==X,B==Y,C>=Z).
overlapPeriod(period(A-B-C,X-Y-Z),period(A2-B2-C2,X2-Y2-Z2)):-
    bigger_Or_Equal_Period(X2,Y2,Z2,A,B,C),bigger_Or_Equal_Period(X,Y,Z,A2,B2,C2).

	