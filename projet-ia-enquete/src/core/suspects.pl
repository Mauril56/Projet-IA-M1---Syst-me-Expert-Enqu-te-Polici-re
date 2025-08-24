:- module(suspects, [
    suspect/5,
    has_motive/2,
    was_near_crime_scene/2,
    has_fingerprint_on_weapon/2,
    has_bank_transaction/2,
    owns_fake_identity/2,
    eyewitness_identification/2
]).

:- dynamic suspect/5.
:- dynamic has_motive/2.
:- dynamic was_near_crime_scene/2.
:- dynamic has_fingerprint_on_weapon/2.
:- dynamic has_bank_transaction/2.
:- dynamic owns_fake_identity/2.
:- dynamic eyewitness_identification/2.

% Format: suspect(Nom, Sexe, Age, Profession, Statut)
suspect('John', homme, 35, 'Comptable', 'actif').
suspect('Mary', femme, 28, 'Secrétaire', 'suspect_principal').
suspect('Alice', femme, 42, 'Directrice', 'actif').
suspect('Bruno', homme, 31, 'Avocat', 'actif').
suspect('Sophie', femme, 45, 'Gardienne', 'actif').


has_motive('John', vol).
was_near_crime_scene('John', vol).
has_fingerprint_on_weapon('John', vol).

has_motive('Mary', assassinat).
was_near_crime_scene('Mary', assassinat).
has_fingerprint_on_weapon('Mary', assassinat).


has_motive('Alice', escroquerie).
has_bank_transaction('Alice', escroquerie).
has_bank_transaction('Bruno', escroquerie).
owns_fake_identity('Sophie', escroquerie).

eyewitness_identification('Mary', assassinat).