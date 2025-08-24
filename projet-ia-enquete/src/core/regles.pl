:- module(regles, [is_guilty/2]).

:- use_module('../core/crimes.pl').
:- use_module('../core/suspects.pl').


is_guilty(Suspect, vol) :-
    suspects:has_motive(Suspect, vol),
    suspects:was_near_crime_scene(Suspect, vol),
    (suspects:has_fingerprint_on_weapon(Suspect, vol)
    ; suspects:eyewitness_identification(Suspect, vol)
    ).


is_guilty(Suspect, assassinat) :-
    suspects:has_motive(Suspect, assassinat),
    suspects:was_near_crime_scene(Suspect, assassinat),
    (suspects:has_fingerprint_on_weapon(Suspect, assassinat)
    ; suspects:eyewitness_identification(Suspect, assassinat)
    ).

is_guilty(Suspect, escroquerie) :-
    (suspects:has_bank_transaction(Suspect, escroquerie)
    ; suspects:owns_fake_identity(Suspect, escroquerie)
    ),
    suspects:has_motive(Suspect, escroquerie).