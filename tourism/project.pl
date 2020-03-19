get_subsets([],[]).
get_subsets([H|T],[H|L2]):-
	get_subsets(T,L2).
get_subsets([H|T],L):-
	get_subsets(T,L).