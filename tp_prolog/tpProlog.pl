symbol(a).
symbol(b).
symbol(c).



/*
tieneEstrella(or(a, b)).                       PASA TEST
false.
?- tieneEstrella(or(concat(star(a), b), c)).   PASA TEST
true.  
?- tieneEstrella(star(concat(a, b))).          PASA TEST
true.
*/
/*  3.1  TIENE ESTRELLA    */
tieneEstrella(star(_)).

tieneEstrella(concat(A, B)) :-
    nonvar(A), nonvar(B),
    (tieneEstrella(A);tieneEstrella(B)).

tieneEstrella(or(A, B)) :-
    (tieneEstrella(A); tieneEstrella(B)).



/*   3.2     longitudMaxima */



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


cadenas([]).

cadenas([S|R]) :-
    symbol(S),        % el símbolo tiene que ser del alfabeto
    cadenas(R).
/*   3.3      cadena */


natural(0).
natural(N) :-
    natural(M),
    N is M + 1.

cadena_de_longitud(0, []).
cadena_de_longitud(N, [S|R]) :-
    N > 0,
    N1 is N - 1,
    symbol(S),
    cadena_de_longitud(N1, R).

cadena(X) :-
    natural(N),
    cadena_de_longitud(N, X).


match_inst([], empty).
match_inst([S], S) :- symbol(S).

match_inst(Cad, concat(A, B)) :-
    append(Cad1, Cad2, Cad),
    match_inst(Cad1, A),
    match_inst(Cad2, B).

match_inst(Cad, or(A, B)) :-
    match_inst(Cad, A);
    match_inst(Cad, B).

match_inst(Cad, star(A)) :-
    (Cad = [];
     append(Prefix, Suffix, Cad),
     Prefix \= [],
     match_inst(Prefix, A),
     match_inst(Suffix, star(A))).




match(Cad, RegEx) :-
    cadena(Cad),
    match_inst(Cad, RegEx).     



diferencia(Cadena, R1, R2) :-
    match(Cadena, R1),
    \+ match_inst(Cadena, R2).
