
insert(E,L,[E|L]).
insert(E,[H|T],[H|A]):-
	insert(E,T,A).
	
perm([],[]).
perm([H|T],A):-
	perm(T,A1),
	insert(H,A1,A).
	
get_subsets([],[]).

get_subsets([H|T],[H|L2]):-
    get_subsets(T,L2).
    
get_subsets([_|T],L):-
    get_subsets(T,L).
	
possibleSubset(L,A):-
	get_subsets(L,A1),
	perm(A1,A).
	