zeroInit(L) :- 
	L = [C | T],
    C =:= 0.

has5(L) :-
	L = [C | T],
	length(L, X), X =:= 5. 

hasN([],0).
hasN(L,N) :-
	L = [X | Y],
	hasN(Y, N1), 
	N is N1 > 0.

potN0(0,[]).
potN0(N,L) :- 
	L = [C|T],
	C is (2^N), 
	N1 is N - 1,
	potN0(N1,T).

zipmult([],[],[]).
zipmult(L1,L2,L3) :-
	L1 = [H1|T1]
	L2 = [H2|T2]
	L3 = [H3|T3]
	H3 is H1 * H2, 
	zimult(T1,T2,T3).

potencias(N,L) :- 
	potencias_aux(N,0,L).

potencias_aux(N1,N2,[]) :- N1 =:= N2.
potencias_aux(N1,N2,L) :-
	L = [H|T] 
	H is (2^N2), 
	P is N2 + 1, 
	potencias_aux(N1,P,T).

positivos([],[]).
positivos(L1,L2) :-
	L1 = [H1|T1]
	L2 = [H2|T2] 
	H1 > 0, H2 is H1,
	positivos(T1,T2).

