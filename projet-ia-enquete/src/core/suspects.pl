% Base de données des suspects et preuves
:- module(suspects, [
    suspect/1,
    list_suspects/0,
    validate_suspect/1,
    has_motive/2,
    was_near_crime_scene/2,
    has_fingerprint_on_weapon/2,
    has_bank_transaction/2,
    owns_fake_identity/2,
    eyewitness_identification/2
]).

% Déclarer les prédicats discontinus
:- discontiguous has_motive/2.
:- discontiguous was_near_crime_scene/2.
:- discontiguous has_fingerprint_on_weapon/2.
:- discontiguous has_bank_transaction/2.
:- discontiguous owns_fake_identity/2.
:- discontiguous eyewitness_identification/2.

% Suspects
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

% Preuves pour le VOL
has_motive(john, vol).
was_near_crime_scene(john, vol).
has_fingerprint_on_weapon(john, vol).

% Preuves pour l'ASSASSINAT
has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).

% Preuves pour l'ESCROQUERIE
has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).
has_bank_transaction(bruno, escroquerie).
owns_fake_identity(sophie, escroquerie).

% Témoin oculaire (exemple supplémentaire)
eyewitness_identification(mary, assassinat).

% Lister tous les suspects
list_suspects :-
    write('Liste des suspects:'), nl,
    forall(suspect(S), format('  - ~w~n', [S])).

% Validation d'un suspect
validate_suspect(Suspect) :-
    suspect(Suspect), !.
validate_suspect(Suspect) :-
    format('Erreur: ~w n\'est pas un suspect connu.~n', [Suspect]),
    fail.