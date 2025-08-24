% Règles d'inférence pour l'enquête
:- module(regles, [
    is_guilty/2,
    has_alibi/2,
    is_innocent/2,
    evidence_strength/3
]).

:- use_module('../core/suspects.pl').

% Règle pour le VOL
is_guilty(Suspect, vol) :-
    suspects:has_motive(Suspect, vol),
    suspects:was_near_crime_scene(Suspect, vol),
    suspects:has_fingerprint_on_weapon(Suspect, vol).

% Règle pour l'ASSASSINAT
is_guilty(Suspect, assassinat) :-
    suspects:has_motive(Suspect, assassinat),
    suspects:was_near_crime_scene(Suspect, assassinat),
    (suspects:has_fingerprint_on_weapon(Suspect, assassinat)
    ; suspects:eyewitness_identification(Suspect, assassinat)
    ).

% Règle pour l'ESCROQUERIE
is_guilty(Suspect, escroquerie) :-
    (suspects:has_motive(Suspect, escroquerie), suspects:has_bank_transaction(Suspect, escroquerie))
    ;
    (suspects:has_bank_transaction(Suspect, escroquerie), suspects:owns_fake_identity(Suspect, escroquerie)).

% Règles supplémentaires
has_alibi(Suspect, Crime) :- 
    \+ suspects:was_near_crime_scene(Suspect, Crime).

is_innocent(Suspect, Crime) :-
    \+ is_guilty(Suspect, Crime).

% Niveaux de preuve
evidence_strength(Suspect, Crime, strong) :-
    suspects:has_fingerprint_on_weapon(Suspect, Crime),
    suspects:has_motive(Suspect, Crime),
    suspects:was_near_crime_scene(Suspect, Crime).

evidence_strength(Suspect, Crime, medium) :-
    (suspects:has_motive(Suspect, Crime), suspects:was_near_crime_scene(Suspect, Crime))
    ;
    (suspects:has_bank_transaction(Suspect, Crime), suspects:owns_fake_identity(Suspect, Crime)).

evidence_strength(Suspect, Crime, weak) :-
    suspects:has_motive(Suspect, Crime)
    ; suspects:was_near_crime_scene(Suspect, Crime)
    ; suspects:has_bank_transaction(Suspect, Crime).