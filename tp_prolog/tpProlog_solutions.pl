% Soluciones sencillas para los ejercicios del TP (forma básica y legible).
% Archivo: tpProlog_solutions.pl
% Hecho como alternativa más simple a las soluciones en tpProlog.pl

% Alfabeto
symbol(a).
symbol(b).
symbol(c).

% 3.1 tieneEstrella(+RegEx)
% Verdadero si la expresion regular contiene al menos una estrella.
% Implementacion directa por patron y recusividad sencilla.

tieneEstrella(star(_)).
tieneEstrella(concat(A,B)) :-
    tieneEstrella(A);
    tieneEstrella(B).
tieneEstrella(or(A,B)) :-
    tieneEstrella(A);
    tieneEstrella(B).


% 3.2 longitudMaxima(+RegEx, -N)
% Maxima longitud de las cadenas aceptadas. Si acepta infinitas, falla.

longitudMaxima(empty, 0).
longitudMaxima(S, 1) :- symbol(S).
longitudMaxima(concat(A,B), N) :-
    longitudMaxima(A, NA),
    longitudMaxima(B, NB),
    N is NA + NB.
longitudMaxima(or(A,B), N) :-
    longitudMaxima(A, NA),
    longitudMaxima(B, NB),
    N is max(NA, NB).
% star genera infinitas cadenas => hacemos que falle
longitudMaxima(star(_), _) :-
    !, fail.


% 3.3 cadena(?X)
% Genera (o verifica) listas cuyos elementos son simbolos del alfabeto.
% Implementacion muy simple: [] es cadena; para construir una, agregamos
% un simbolo delante recursivamente.

cadena([]).
cadena([S|R]) :-
    symbol(S),
    cadena(R).


% 3.4 match_inst(+Cadena,+RegEx)
% Predicado que verifica si una cadena concreta es aceptada por la expresion
% regular. Implementacion directa por casos.

% vacio
match_inst([], empty).
% simbolo unico
match_inst([X], X) :- symbol(X).
% concatenacion: dividimos la lista en dos partes
match_inst(Xs, concat(R1, R2)) :-
    append(A, B, Xs),
    match_inst(A, R1),
    match_inst(B, R2).
% alternacion
match_inst(Xs, or(R1, R2)) :-
    ( match_inst(Xs, R1) ; match_inst(Xs, R2) ).
% estrella: cero repeticiones o una repeticion de R seguida de mas estrella
match_inst([], star(_)).
match_inst(Xs, star(R)) :-
    Xs \= [],
    append(A, B, Xs),
    A \= [],
    match_inst(A, R),
    match_inst(B, star(R)).


% 3.5 match(?Cadena,+RegEx)
% Extiende match_inst para poder generar cadenas aceptadas por RegEx.
% Estrategia sencilla:
% - Si Cadena ya esta instanciada, delegamos en match_inst.
% - Si no, intentamos generar cadenas con la estrategia "crece recursivamente"
%   usando la generadora simple `cadena/1`. Para expresiones con longitud maxima
%   conocida (sin star) usamos longitudMaxima para acotar la generacion.

match(Cadena, RegEx) :-
    nonvar(Cadena),
    match_inst(Cadena, RegEx).

match(Cadena, RegEx) :-
    var(Cadena),
    (   longitudMaxima(RegEx, Max) ->
        % si tiene maxima, probamos longitudes 0..Max
        between(0, Max, L),
        length(Cadena, L),
        cadena(Cadena),
        match_inst(Cadena, RegEx)
    ;
        % si no tiene maxima, asumimos que tiene estrella y generamos sin tope
        cadena(Cadena),
        match_inst(Cadena, RegEx)
    ).


% 3.6 diferencia(?Cadena, +R1, +R2)
% Verdadero si Cadena es aceptada por R1 y NO es aceptada por R2.

diferencia(Cadena, R1, R2) :-
    match(Cadena, R1),
    \+ match(Cadena, R2).

% Fin del archivo
