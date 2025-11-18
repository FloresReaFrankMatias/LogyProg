symbol(a).
symbol(b).
symbol(c).


/*  3.1 tieneEstrella(+RegEx)*/
tieneEstrella(star(_)).

tieneEstrella(concat(A, B)) :-
    nonvar(A), nonvar(B),
    (tieneEstrella(A);tieneEstrella(B)).

tieneEstrella(or(A, B)) :-
    (tieneEstrella(A); tieneEstrella(B)).



/*   3.2 longitudMaxima(+RegEx, -N)*/



longitudMaxima(empty, 0).
longitudMaxima(S, 1) :-
    symbol(S).

longitudMaxima(concat(A, B), N) :-
    longitudMaxima(A, N1),
    longitudMaxima(B, N2),
    N is N1 + N2.

longitudMaxima(or(A, B), N) :-
    longitudMaxima(A, N1),
    longitudMaxima(B, N2),
    N is max(N1, N2).

longitudMaxima(star(_), _) :- fail.



/*   3.3      cadena */


logitud(0).
logitud(N) :-
    logitud(M),
    N is M + 1.

cadena_de_longitud(0, []).
cadena_de_longitud(N, [S|R]) :-
    N > 0,
    N1 is N - 1,
    symbol(S),
    cadena_de_longitud(N1, R).

cadena(X) :-
    logitud(N),
    cadena_de_longitud(N, X).

    

/*  3.4 match inst(+Cadena,+RegEx)*/
match_inst([], empty).

match_inst([X], X) :- symbol(X).

match_inst(Xs, concat(R1, R2)) :-
    append(A, B, Xs),
    match_inst(A, R1),
    match_inst(B, R2).

match_inst(Xs, or(R1, R2)) :-
    match_inst(Xs, R1),!;
    match_inst(Xs, R2),!.
match_inst([], star(_)).
match_inst(Cadena, star(R)) :-
    append(Parte1, Parte2, Cadena),
    Parte1 \= [],              % evita bucle infinito
    match_inst(Parte1, R),
    match_inst(Parte2, star(R)).

/*  3.5 match(?Cadena,+RegEx)*/
match(Cadena, RegEx) :-
    nonvar(Cadena),
    match_inst(Cadena, RegEx).

match(Cadena, RegEx) :-
    var(Cadena),
    longitudMaxima(RegEx, Max),
    between(0, Max, Len),
    cadena_de_longitud(Len, Cadena),
    match_inst(Cadena, RegEx).

match(Cadena, RegEx) :-
    var(Cadena),
    \+ longitudMaxima(RegEx, _),
    cadena(Cadena),
    match_inst(Cadena, RegEx).

/*  3.6 diferencia(?Cadena, +R1, +R2) */

diferencia(Cadena, R1, R2) :-
    match(Cadena, R1),
    \+ match_inst(Cadena, R2).

