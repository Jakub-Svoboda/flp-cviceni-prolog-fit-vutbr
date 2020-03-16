% FLP CVICENI 4 - PROLOG 1 - UVOD

% ukazka predikatu pro vypocet funkce faktorial
factorial( 0, 1 ).
factorial( N, Value ) :-
     N > 0,
     Prev is N - 1,
     factorial( Prev, Prevfact ),
     Value is Prevfact * N.

% databaze rodinnych vztahu
muz(jan).
muz(pavel).
muz(robert).
muz(tomas).
muz(petr).

zena(marie).
zena(jana).
zena(linda).
zena(eva).

otec(tomas,jan).
otec(jan,robert).
otec(jan,jana).
otec(pavel,linda).
otec(pavel,eva).

matka(marie,robert).
matka(linda,jana).
matka(eva,petr).
matka(eva,adam).

% Implementujte nasledujici predikaty:

rodic(R,X) :- otec(R,X); matka(R,X).
sourozenec(X,Y) :- otec(O,X), otec(O,Y), not(X=Y).
sourozenec(X,Y) :- matka(M,X), matka(M,Y), not(X=Y).
sestra(Sis,X) :- zena(Sis), sourozenec(Sis,X).
deda(X,Y) :- muz(X), rodic(X,Z), rodic(Z,Y).
je_matka(X) :- zena(X), matka(X,_).
teta(Tet,X) :- sestra(Tet,R), otec(R,X).
teta(Tet,X) :- sestra(Tet,R), matka(R,X). 


% Seznamy:
neprazdny([_|_]) :- true.
hlavicka([H|_], H).
posledni([H], H) :- !.
posledni([_|T], Res) :- posledni(T, Res).


% Dalsi ukoly:
spoj([], L, L).
spoj([X|T1], L2, [X|T2]) :- spoj(T1, L2, T2).
obrat([],[]).
obrat([H|T], Res) :- obrat(T, TRes), spoj(TRes, [H], Res).

sluc([],[],[]).
sluc([X],[],[X]).
sluc([],[Y],[Y]).
sluc([X|L1],[Y|L2],[X|L]) :- 
    X =< Y,!, 
    sluc(L1,[Y|L2],L).
sluc([X|L1],[Y|L2],[Y|L]) :- 
    sluc([X|L1],L2,L).

serad([], []).
serad([X],[X]).
serad(L,R):-
    L = [_,_|_], sudaLicha(L,L1,L2),     %merge sort
	serad(L1,R1), serad(L2,R2),  
	sluc(R1,R2,R).                  

sudaLicha([],[],[]).
sudaLicha([H|T],E,[H|O]):-sudaLicha(T,O,E).

plus(X,Y,Z) :- Z is X + Y.
minus(X,Y,Z) :- Z is X - Y.
mult(X,Y,Z) :- Z is X * Y.

zipWith(_, [], _, []) :- !.
zipWith(_, _, [], []) :- !.    
zipWith(O,[H1|T1],[H2|T2],[H3|R]) :-
  X =.. [O, H1, H2, H3],
  call(X),
  zipWith(O, T1, T2, R).