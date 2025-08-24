:- module(cli_interface, [start_cli/0]).

:- use_module('../core/enquete_core.pl').
:- use_module('../core/ia_advanced.pl').
:- use_module('../utils/helpers.pl').

% Interface en ligne de commande interactive
start_cli :-
    helpers:clear_screen,
    write('================================='), nl,
    write('  SYSTÈME IA - ENQUÊTE'), nl,
    write('  Interface en ligne de commande'), nl,
    write('================================='), nl, nl,
    
    cli_main_loop.

cli_main_loop :-
    repeat,
    display_menu,
    read_line_to_string(user_input, Line),
    (Line = "" -> Choice = 0 ; atom_number(Line, Choice)),
    process_choice(Choice),
    Choice = 9.

display_menu :-
    nl,
    write('┌─────────────────────────────┐'), nl,
    write('│          MENU PRINCIPAL     │'), nl,
    write('├─────────────────────────────┤'), nl,
    write('│ 1. Analyser un suspect      │'), nl,
    write('│ 2. Voir tous les suspects   │'), nl,
    write('│ 3. Test IA avancé           │'), nl,
    write('│ 4. Arbre de preuves         │'), nl,
    write('│ 5. Scénario hypothétique    │'), nl,
    write('│ 6. Résolution de conflits   │'), nl,
    write('│ 7. Exécuter tous les tests  │'), nl,
    write('│ 8. Réinitialiser système    │'), nl,
    write('│ 9. Quitter                  │'), nl,
    write('└─────────────────────────────┘'), nl, nl,
    write('Votre choix (1-9): '),
    flush_output.

process_choice(1) :- analyze_suspect.
process_choice(2) :- list_all_suspects.
process_choice(3) :- test_ai_features.
process_choice(4) :- show_proof_tree.
process_choice(5) :- hypothetical_scenario.
process_choice(6) :- resolve_conflicts.
process_choice(7) :- run_all_tests.
process_choice(8) :- reset_system.
process_choice(9) :- write('Au revoir!'), nl.
process_choice(_) :- write('Choix invalide. Réessayez.'), nl, fail.

analyze_suspect :-
    nl, write('Nom du suspect: '), flush_output,
    read_line_to_string(user_input, SuspectAtom),
    atom_string(SuspectRaw, SuspectAtom),
    downcase_atom(SuspectRaw, Suspect),  % Conversion en minuscules
    write('Type de crime: '), flush_output,
    read_line_to_string(user_input, CrimeAtom),
    atom_string(CrimeRaw, CrimeAtom),
    downcase_atom(CrimeRaw, Crime),      % Conversion en minuscules
    ia_advanced:explain_verdict(Suspect, Crime),
    helpers:press_enter_to_continue.

list_all_suspects :-
    nl, write('📋 LISTE DES SUSPECTS:'), nl,
    write('======================='), nl,
    forall(suspects:suspect(Name, Gender, Age, Profession, Status),
           format('• ~w (~w, ~w ans) - ~w [~w]~n', 
                  [Name, Gender, Age, Profession, Status])),
    helpers:press_enter_to_continue.

test_ai_features :-
    nl, write('🧪 TEST DES FONCTIONNALITÉS IA:'), nl,
    write('==============================='), nl,
    ia_advanced:explain_verdict(john, vol),
    ia_advanced:explain_verdict(mary, assassinat),
    ia_advanced:explain_verdict(alice, escroquerie),
    helpers:press_enter_to_continue.

show_proof_tree :-
    nl, write('Nom du suspect: '), flush_output,
    read_line_to_string(user_input, SuspectAtom),
    atom_string(Suspect, SuspectAtom),
    write('Type de crime: '), flush_output,
    read_line_to_string(user_input, CrimeAtom),
    atom_string(Crime, CrimeAtom),
    ia_advanced:proof_tree(Suspect, Crime),
    helpers:press_enter_to_continue.

hypothetical_scenario :-
    nl, write('Nom du suspect: '), flush_output,
    read_line_to_string(user_input, SuspectAtom),
    atom_string(Suspect, SuspectAtom),
    write('Type de crime: '), flush_output,
    read_line_to_string(user_input, CrimeAtom),
    atom_string(Crime, CrimeAtom),
    write('Preuves à ajouter (une par ligne, entrée vide pour terminer): '), nl,
    flush_output,
    
    read_multiple_proofs(Proofs),
    
    maplist(assert_proof, Proofs),
    
    format('~n🤔 SCÉNARIO HYPOTHÉTIQUE pour ~w (~w)~n', [Suspect, Crime]),
    format('Preuves supplémentaires: ~w~n~n', [Proofs]),
    
    
    (regles:is_guilty(Suspect, Crime) -> 
        format('📌 ÉTAT ACTUEL: COUPABLE~n', [])
    ; 
        format('📌 ÉTAT ACTUEL: INNOCENT~n', [])
    ),
    
    format('~n📊 AVEC LES PREUVES SUPPLEMENTAIRES:~n', []),
    (regles:is_guilty(Suspect, Crime) ->
        format('📌 RÉSULTAT: COUPABLE~n', [])
    ;
        format('📌 RÉSULTAT: INNOCENT~n', [])
    ),
    
    maplist(retract_proof, Proofs),
    
    helpers:press_enter_to_continue.

assert_proof(Proof) :-
    (callable(Proof) -> assertz(Proof) ; true).

retract_proof(Proof) :-
    (callable(Proof) -> retract(Proof) ; true).

read_multiple_proofs(Proofs) :-
    read_multiple_proofs([], Proofs).

read_multiple_proofs(Acc, Proofs) :-
    write('> '), flush_output,
    read_line_to_string(user_input, Line),
    string_chars(Line, Chars),
    exclude(=(' '), Chars, CleanChars),
    string_chars(TrimmedLine, CleanChars),
    (TrimmedLine == "" ->  
        reverse(Acc, Proofs)
    ;
        (term_string(Proof, TrimmedLine) ->
            read_multiple_proofs([Proof|Acc], Proofs)
        ;
            format('Erreur: "~w" n\'est pas une preuve valide~n', [TrimmedLine]),
            read_multiple_proofs(Acc, Proofs)
        )
    ).

resolve_conflicts :-
    nl, write('Nom du suspect: '), flush_output,
    read_line_to_string(user_input, SuspectAtom),
    atom_string(Suspect, SuspectAtom),
    ia_advanced:conflict_resolution(Suspect),
    helpers:press_enter_to_continue.

run_all_tests :-
    nl, write('🧪 EXÉCUTION DE TOUS LES TESTS:'), nl,
    write('==============================='), nl,
    ['../tests/test_system.pl'],
    test_system:run_all_tests,
    helpers:press_enter_to_continue.

reset_system :-
    nl, write('🔄 RÉINITIALISATION DU SYSTÈME...'), nl,
    retractall(suspects:suspect(_, _, _, _, _)),
    retractall(suspects:has_motive(_, _)),
    retractall(suspects:was_near_crime_scene(_, _)),
    retractall(suspects:has_fingerprint_on_weapon(_, _)),
    retractall(suspects:has_bank_transaction(_, _)),
    retractall(suspects:owns_fake_identity(_, _)),
    
    
    consult('../core/suspects.pl'),
    consult('../core/crimes.pl'),
    write('✅ Système réinitialisé'), nl,
    helpers:press_enter_to_continue.

read_line_to_string(Stream, String) :-
    read_line_to_codes(Stream, Codes),
    (Codes == end_of_file -> String = "" ; atom_codes(String, Codes)).