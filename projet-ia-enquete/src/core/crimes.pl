% Définition des types de crimes
:- module(crimes, [
    crime_type/1,
    list_crimes/0,
    validate_crime/1
]).

crime_type(assassinat).
crime_type(vol).
crime_type(escroquerie).

% Lister tous les crimes
list_crimes :-
    write('Types de crimes disponibles:'), nl,
    forall(crime_type(Crime), format('  - ~w~n', [Crime])).

% Validation d'un crime
validate_crime(Crime) :-
    crime_type(Crime), !.
validate_crime(Crime) :-
    format('Erreur: ~w n\'est pas un type de crime valide.~n', [Crime]),
    fail.