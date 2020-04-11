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
contains([H|T],E):-
	H = E;
	contains(T,E).
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
	((offerMean(Offer,X),S is R+S1);((\+offerMean(Offer,X)) , S is S1)).
	
preferenceSatisfaction(Offer, Customer, [accommodation(X)|T], S):-
		preferenceSatisfaction(Offer, Customer, T, S1),
		customerPreferredAccommodation(Customer,X,R),
		((offerAccommodation(Offer,X),S is R+S1);((\+offerAccommodation(Offer,X)) , S is S1)).
				
preferenceSatisfaction(offer(D,A,C,Vf,Vt,P,Du,G), Customer, [activity([X|Y])|T], S):-
	preferenceSatisfaction(offer(D,A,C,Vf,Vt,P,Du,G), Customer,[activity(Y)|T], S1),
	customerPreferredActivity(Customer,X,R),
	((contains(A,X),S is R + S1);((\+contains(A,X),S is S1))).
	
preferenceSatisfaction(Offer, Customer, [X|T], S):-
	X \= means(_),
	X \= accommodation(_),
	X \= activity([_|_]),
	preferenceSatisfaction(Offer, Customer,T, S).
getOffer([],offer(D,A,C,Vf,Vt,P,Du,G)):-
	offerMean(offer(D,A,C,Vf,Vt,P,Du,G),_).
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
	%recommendOfferForCustomer(_, [], []).
recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
    possibleSubset(Prefs,ChosenPrefs),
    getOffer(ChosenPrefs,O).
getMax([],[],_,_,0).
getMax([H1|T1],[H2|T2],Offer,N,Max):-
	 (
	preferenceSatisfaction(Offer, H1, H2, S),
	N1 is N-1,
	getMax(T1,T2,Offer,N1,NewMax1),
	getMax(T1,T2,Offer,N,NewMax2),
	X is S + NewMax1,
	((X>=NewMax2 ,Max is X,N>0);((N=<0;X<NewMax2),Max is NewMax2))).
equalsMax([],[],_,0,_,[]).
equalsMax([H1|T1],[H2|T2],Offer,N,Max,[H1|Y]):-
	N>0,(
	preferenceSatisfaction(Offer, H1, H2, S),
	N1 is N-1,
	getMax(T1,T2,Offer,N1,NewMax1),
	Max is S + NewMax1,
	equalsMax(T1,T2,Offer,N1,NewMax1,Y)).
equalsMax([H1|T1],[H2|T2],Offer,N,Max,Y):-
	preferenceSatisfaction(Offer, H1, H2, S),
	getMax(T1,T2,Offer,N,NewMax1),
	Max is NewMax1,
	equalsMax(T1,T2,Offer,N,Max,Y).	
len([],0).
len([_|T],L):-
	len(T,L1),
	L is L1 +1.
	
	
	
recommendOffer(Customers, PreferenceList, Offer, CustomersChosen) :-
	offerMean(Offer,_),
	Offer = offer(_, _, _, _,_,_ ,_, N),
	len(Customers,L),
	((L<N,M = L) ; (L>=N,M=N)),
	getMax(Customers,PreferenceList,Offer,M,Max),
	equalsMax(Customers,PreferenceList,Offer,M,Max,CustomersChosen).