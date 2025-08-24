:- module(api, [
    analyze_suspect/3,
    get_suspects/1,
    get_crimes/1,
    run_hypothesis/4
]).

:- use_module('../core/enquete_core.pl').
:- use_module('../core/ia_advanced.pl').

analyze_suspect(Suspect, Crime, Result) :-
    (regles:is_guilty(Suspect, Crime) ->
        Result = guilty ;
        Result = innocent).

get_suspects(Suspects) :-
    findall(Name, suspects:suspect(Name, _, _, _, _), Suspects).

get_crimes(Crimes) :-
    findall(Crime, crimes:crime_type(Crime), Crimes).

run_hypothesis(Suspect, Crime, AdditionalProof, Result) :-
    assertz(AdditionalProof),
    (regles:is_guilty(Suspect, Crime) ->
        Result = guilty ;
        Result = innocent),
    retract(AdditionalProof).