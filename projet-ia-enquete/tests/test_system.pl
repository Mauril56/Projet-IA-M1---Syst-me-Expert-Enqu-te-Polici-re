% Script de test du système complet
:- use_module('src/core/crimes.pl').
:- use_module('src/core/suspects.pl').
:- use_module('src/core/regles.pl').
:- use_module('src/core/ia_advanced.pl').

% Test de base
test_basic_functionality :-
    write('=== TEST DU SYSTÈME DE BASE ==='), nl,
    
    % Test 1: John coupable de vol
    (regles:is_guilty(john, vol) ->
        write('✅ Test 1 réussi: John est coupable de vol'), nl
    ;
        write('❌ Test 1 échoué: John devrait être coupable de vol'), nl
    ),
    
    % Test 2: Mary coupable d'assassinat
    (regles:is_guilty(mary, assassinat) ->
        write('✅ Test 2 réussi: Mary est coupable d\'assassinat'), nl
    ;
        write('❌ Test 2 échoué: Mary devrait être coupable d\'assassinat'), nl
    ),
    
    % Test 3: Alice coupable d'escroquerie
    (regles:is_guilty(alice, escroquerie) ->
        write('✅ Test 3 réussi: Alice est coupable d\'escroquerie'), nl
    ;
        write('❌ Test 3 échoué: Alice devrait être coupable d\'escroquerie'), nl
    ),
    
    % Test 4: Bruno innocent
    (\+ regles:is_guilty(bruno, vol), \+ regles:is_guilty(bruno, assassinat) ->
        write('✅ Test 4 réussi: Bruno est innocent'), nl
    ;
        write('❌ Test 4 échoué: Bruno devrait être innocent'), nl
    ).

% Test des fonctionnalités IA
test_ai_features :-
    write('=== TEST DES FONCTIONNALITÉS IA ==='), nl,
    
    % Test du système d'explication
    write('Test du système d\'explication:'), nl,
    ia_advanced:explain_verdict(john, vol),
    
    % Test du niveau de certitude
    write('Test du niveau de certitude:'), nl,
    (ia_advanced:certainty_level(john, vol, Certainty) ->
        format('Certitude pour John/vol: ~2f%~n', [Certainty*100])
    ;
        write('Erreur dans le calcul de certitude'), nl
    ),
    
    % Test de l'arbre de preuves
    write('Test de l\'arbre de preuves:'), nl,
    ia_advanced:proof_tree(mary, assassinat).

% Test complet
run_all_tests :-
    write('🧪 DÉBUT DES TESTS SYSTÈME'), nl,
    write('=' * 40), nl,
    test_basic_functionality,
    nl,
    test_ai_features,
    nl,
    write('🎯 TESTS TERMINÉS'), nl,
    write('=' * 40), nl.

% Point d'entrée
main :- run_all_tests.