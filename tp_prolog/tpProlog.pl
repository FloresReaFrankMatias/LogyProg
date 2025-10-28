symbol(a).
symbol(b).
symbol(c).


/*  3.1 tieneEstrella(+RegEx)
    Definir el predicado tieneEstrella(+RegEx) que es verdadero cuando la 
    expresion regular RegEx contiene al menos una estrella
  */
tieneEstrella(star(_)).

tieneEstrella(concat(A, B)) :-
    nonvar(A), nonvar(B),
    (tieneEstrella(A);tieneEstrella(B)).

tieneEstrella(or(A, B)) :-
    (tieneEstrella(A); tieneEstrella(B)).



/*   3.2 longitudMaxima(+RegEx, -N)
    Definir el predicado longitudMaxima(+RegEx, -N) que es verdadero cuando
    las cadenas aceptadas por la expresi ́on regular RegEx tienen longitud m ́axima
    N. Observemos que si RegEx acepta cadenas infinitas, el predicado debe fallar.
 */



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



/*   3.3      cadena 
   Definir el predicado cadena(?X) que es verdadero cuando X es una lista de los
   s ́ımbolos del alfabeto. En caso de que X no est ́e instanciada, este predicado debe
    generar todas las cadenas posibles (sin repeticiones).
*/


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

/*  3.4 match inst(+Cadena,+RegEx)
    Definir el predicado match_inst(+Cadena,+RegEx) que, dada una 
    cadena y una expresi on regular, es verdadero cuando Cadena es 
    aceptada por RegEx. 
*/
/* Caso base: cadena vacía y expresión vacía */
match_inst([], empty).

/* Caso base: un símbolo simple*/
match_inst([X], X) :- symbol(X).

/* Concatenación: la lista es A seguido de B */
match_inst(Xs, concat(R1, R2)) :-
    append(A, B, Xs),
    match_inst(A, R1),
    match_inst(B, R2).

/* % Alternativa: coincide con alguno de los dos*/
match_inst(Xs, or(R1, R2)) :-
    match_inst(Xs, R1),!;
    match_inst(Xs, R2),!.

/* % Estrella: cero o mas repeticiones*/
match_inst(Xs, star(R)) :-
    Xs = [] ;
    (Xs = [_|_],
     append(A, B, Xs),
     A \= [],
     match_inst(A, R),
     match_inst(B, star(R))),!.


/*  3.5 match(?Cadena,+RegEx)
    Definir el predicado match(?Cadena,+RegEx) que extienda match_inst para
    que adem ́as pueda generar todas las cadenas aceptadas por la expresion
    regular RegEx.
*/

match(Cadena, RegEx) :-
    nonvar(Cadena),
    match_inst(Cadena, RegEx).

match(Cadena, RegEx) :-
    var(Cadena),
    longitudMaxima(RegEx, N),
    between(0, N, L),
    cadena_de_longitud(L, Cadena),
    match_inst(Cadena, RegEx).

match(Cadena, RegEx) :-
    var(Cadena),
    tieneEstrella(RegEx),
    natural(L),
    cadena_de_longitud(L, Cadena),
    match_inst(Cadena, RegEx).    



/* otra forma que encontre d ehacerlo*/

match2(Cad, RegEx) :-  
    longitudMaxima(RegEx, M), 
    between(0, M, N),
	cadena_de_longitud(N, Cad), 
    match_inst(Cad, RegEx).
match2(Cad, RegEx) :-  
    not(longitudMaxima(RegEx,_)), 
    cadena(Cad), 
    match_inst(Cad, RegEx).

/* 3.6 diferencia(?Cadena, +R1, +R2)
    Definir el predicado diferencia(?Cadena, +R1, +R2) que es verdadero cuando
    Cadena es aceptada por la expresi ́on regular R1, y no es aceptada por la
    expresion regular R2
*/
diferencia(Cadena, R1, R2) :-
    match(Cadena, R1),
    \+ match_inst(Cadena, R2).
