% Validation des données
:- module(validations, [
    validate_input/2,
    is_valid_suspect/1,
    is_valid_crime/1
]).

:- use_module('../core/suspects.pl').
:- use_module('../core/crimes.pl').

validate_input(Suspect, Crime) :-
    is_valid_suspect(Suspect),
    is_valid_crime(Crime).

is_valid_suspect(Suspect) :-
    suspect(Suspect).

is_valid_crime(Crime) :-
    crime_type(Crime).