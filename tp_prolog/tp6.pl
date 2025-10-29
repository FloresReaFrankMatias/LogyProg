symbol(0).

natural(0).
natural(suc(X)) :- natural(X).

menorOIgual(X, X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

concatenar([], L, L).
concatenar([H|T], L, [H|R]) :- concatenar(T, L, R).

last([X], X).
last([_|T], X):- last(T, X).

reverse2([], []).
reverse2([H|T], RL) :- reverse2(T, RLT), concatenar(RLT, [H], RL ).
/*  */
max_list([],_):-fail.
max_list([M],M).

max_list([Head|Tail],Max) :-
    max_list(Tail,TailMax),
    Head > TailMax,
    Max is Head.

max_list([Head|Tail],Max) :-
    max_list(Tail,TailMax),
    Head =< TailMax,
    Max is TailMax.
min_list([],_):-fail.
min_list([M],M).
min_list([Head|Tail],Min) :-
    min_list(Tail,TailMax),
    Head =< TailMax,
    Min is Head.
min_list([Head|Tail],Min) :-
    min_list(Tail,TailMin),
    Head > TailMin,
    Min is TailMin.


max_in_list([],_):- fail.
max_in_list([M],M).                 

max_in_list([H,K|T],M) :-
    H > K,                          
    max_in_list([H|T],M).              
max_in_list([H,K|T],M) :-
    H =< K,                            
    max_in_list([K|T],M).

min_in_list([],_):- fail.
min_in_list([Min],Min).                 
min_in_list([H,K|T],M) :-
    H =< K,                             
    min_in_list([H|T],M).             
min_in_list([H,K|T],M) :-
    H > K,                            
    min_in_list([K|T],M).

prefijo2(P, L):- concatenar(P, _Resto, L).
sufijo(Xs,Xs). % son iguales
sufijo(Xs,[_|YsXs]) :- sufijo(Xs,YsXs).



pertenece(_,[]):- fail.
pertenece(X, [X|_]).
pertenece(X, [_|T]):-  pertenece(X, T).

