

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

preferenceSatisfaction(_, _, [], 0).

preferenceSatisfaction(Offer, Customer, [means(X)|T], S):-
	preferenceSatisfaction(Offer, Customer, T, S1),
	customerPreferredMean(Customer,X,R),
	S is R+S1.
	
preferenceSatisfaction(Offer, Customer, [accommodation(X)|T], S):-
		preferenceSatisfaction(Offer, Customer, T, S1),
		customerPreferredAccommodation(Customer,X,R),
		S is R+S1.
				
preferenceSatisfaction(Offer, Customer, [activity([X|Y])|T], S):-
	preferenceSatisfaction(Offer, Customer,[activity(Y)|T], S1),
	customerPreferredActivity(Customer,X,R),
	S is R + S1.
	
preferenceSatisfaction(Offer, Customer, [X|T], S):-
	X \= means(_),
	X \= accommodation(_),
	X \= activity([_|_]),
	preferenceSatisfaction(Offer, Customer,T, S).

getOffer([],offer(_,_,_,_,_,_,_,_)).
getOffer([activity(X)|T],offer(D,A,C,Vf,Vt,P,Du,G)):-
	offerMean(offer(D,A,C,Vf,Vt,P,Du,G),_),
	possibleSubset(A,X),getOffer(T,offer(D,A,C,Vf,Vt,P,Du,G)).

getOffer([dest(X)|T],offer(D,A,C,Vf,Vt,P,Du,G)):-
	offerMean(offer(D,A,C,Vf,Vt,P,Du,G),_),
	X=D,getOffer(T,offer(D,A,C,Vf,Vt,P,Du,G)).

getOffer([budget(X)|T],offer(D,A,C,Vf,Vt,P,Du,G)):-
	offerMean(offer(D,A,C,Vf,Vt,P,Du,G), _ ) ,
	C =< X,getOffer(T,offer(D,A,C,Vf,Vt,P,Du,G)).

getOffer([period(X,Y)|T],offer(D,A,C,Vf,Vt,P,Du,G)):-
	offerMean(offer(D,A,C,Vf,Vt,P,Du,G),_),
	overlapPeriod(period(X,Y),P),getOffer(T,offer(D,A,C,Vf,Vt,P,Du,G)).

	
	