% FLP - cviceni 6
% Jakub Svoboda - xsvobo0z
% 19.4.2020


:- dynamic velikost/2, pos/2.


prvek(H, [H|_]) :- !.               %pokud je prvni, ukoncime backtracking
prvek(H, [_|T]) :-                  %jinak zkoumame zbytek pole
	prvek(H,T)
.


rozdil([], _, []).                  %Rozdil z prazdne mnoziny bude vzdy prazdna mnozina
rozdil([H|T], S, R) :- 				%Pokud je prvni prvek prvni mnoziny ve druhe mnozine, do R se neprida
	prvek(H,S),					
	rozdil(T,S,R)
.         
rozdil([H|T], S, [H|P]) :-			%Pokud prvni prvek mnoziny neni v druhe mnozine, je vlozen do vysledku
	not(prvek(H,S)),
	rozdil(T,S,P)
.


sequence(0, []) :- !.				%Pokud N je nula, uz nic negenerujeme
sequence(N, [N|T]) :- 				%Pridame N do seznamu, pokracujeme s N-1
	NLower is N-1,
	sequence(NLower,T)
.

queens(Solution) :- queens(8, Solution).		%8 je default
queens(N, Solution) :-
	sequence(N,Res),							%Generovani listu 1..N
	permutation(Res, Solution),						
	test(Solution)
.		
		
%test(Solution).
test([]) :- !.
test([H|T]) :-
	test(H, 1, T),					%Nejprve testujeme vzdalenost 1 (sousedni sloupce)
	test(T)
.
test(_, _, []) :- !.
test(Pos, Dist, [H|T]) :-
	Pos =\= H,						%redundantni
	Dist =\= (Pos-H),				%Rozdil nesmi byt +-Dist
	Dist =\= -(Pos-H),					
	NewDist is Dist+1,				%Rekurzivne volame na dalso prvek seznamu
	test(Pos, NewDist, T)
.



%XR YR = rozmery sachovnice
%XS YS = pocatecni pozice
%XE YS = cilova pozice
%N = vysledek - pocet moznych cest
cesty(XR, YR, XS, YS, XE, YE, N) :-
	XR > 0,											%Rozmery sachovnice musi byt rozumne
	YR > 1,
	assert(velikost(XR, YR)),						%ulozime velikost sachovnice
	testPoz(XS,YS),									%pozadovany cil a start by mel byt rozumny
	testPoz(XE,YE),
	findall(Res, cesta(XS,YS,XE,YE,Res), X),
	length(X,N),									
	retract(velikost(_,_)),							%vycisteni databaze
	retractall(pos(_,_))
	%write(N),nl
.


testPoz(X,Y) :-
	velikost(XR,YR),
	X > 0, 
	X =< XR,
	Y > 0, 
	Y =< YR
.


skok(X,Y,XN,YN) :- XN is X + 2, YN is Y + 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 2, YN is Y - 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 2, YN is Y + 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 2, YN is Y - 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 1, YN is Y + 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 1, YN is Y - 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 1, YN is Y + 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 1, YN is Y - 2, testPoz(XN, YN).



cesta(X,Y,X,Y,[X:Y]).					%Aktualni pozice je rovna cilove
cesta(X,Y,X,Y,[X:Y]) :- !, fail.
cesta(X,Y,XE,YE,[X:Y|T]) :-
	assert(pos(X,Y)),
	skok(X,Y,XN,YN),
	\+ pos(XN,YN),
	cesta(XN,YN,XE,YE,T)
.	
cesta(X, Y, _, _, _) :-
	retract(pos(X,Y)),
	!,fail
.


% kontroly
slovnik(D, _, _) :- var(D),!,fail.				%nesmyslne kombinace - selze
slovnik(_, K, V) :- var(K),var(V),!,fail.
% vyhledani hodnoty
slovnik(D, K, V) :- var(K), call(D,K,V).
% vyhledani klicu
slovnik(D, K, V) :- var(V), call(D,K,V).
% modifikace
slovnik(D, K, V) :-
	P =.. [D,K,V],
	retractall(P),
	assert(P)
.	
% vlozeni
slovnik(D, K, V) :- 
	P =.. [D,K,V],			
	assert(P)
.

