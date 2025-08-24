% Interface ligne de commande
:- module(cli_interface, [
    start_cli/0,
    display_menu/0,
    process_choice/1
]).

:- use_module('../core/crimes.pl').
:- use_module('../core/suspects.pl').
:- use_module('../core/regles.pl').
:- use_module('../utils/helpers.pl').

start_cli :-
    display_welcome,
    main_loop.

display_welcome :-
    write('================================='), nl,
    write('  INTERFACE CLI - ENQUÊTE POLICE'), nl,
    write('================================='), nl, nl.

main_loop :-
    repeat,
    display_menu,
    read_choice(Choice),
    process_choice(Choice),
    (Choice =:= 6 -> ! ; true).

display_menu :-
    write('=== MENU PRINCIPAL ==='), nl,
    write('1. Vérifier culpabilité d\'un suspect'), nl,
    write('2. Lister tous les suspects'), nl,
    write('3. Lister tous les crimes'), nl,
    write('4. Investigation complète'), nl,
    write('5. Explication détaillée'), nl,
    write('6. Quitter'), nl, nl,
    write('Votre choix (1-6): ').

read_choice(Choice) :-
    read(Choice),
    integer(Choice),
    between(1, 6, Choice).

process_choice(1) :-
    nl, write('=== VÉRIFICATION CULPABILITÉ ==='), nl,
    write('Nom du suspect: '), read(Suspect),
    write('Type de crime: '), read(Crime),
    (validate_suspect(Suspect), validate_crime(Crime) ->
        (is_guilty(Suspect, Crime) ->
            format('✅ ~w est COUPABLE de ~w.~n', [Suspect, Crime])
        ;
            format('✅ ~w est INNOCENT de ~w.~n', [Suspect, Crime])
        )
    ;
        write('❌ Données invalides. Veuillez réessayer.~n')
    ),
    nl.

process_choice(2) :-
    nl, write('=== LISTE DES SUSPECTS ==='), nl,
    list_suspects, nl.

process_choice(3) :-
    nl, write('=== TYPES DE CRIMES ==='), nl,
    list_crimes, nl.

process_choice(4) :-
    nl, write('=== RAPPORT COMPLET ==='), nl,
    generate_complete_report, nl.

process_choice(5) :-
    nl, write('=== EXPLICATION DÉTAILLÉE ==='), nl,
    write('Nom du suspect: '), read(Suspect),
    write('Type de crime: '), read(Crime),
    (validate_suspect(Suspect), validate_crime(Crime) ->
        explain_verdict(Suspect, Crime)
    ;
        write('❌ Données invalides.~n')
    ),
    nl.

process_choice(6) :-
    nl, write('👋 Au revoir ! Enquête terminée.'), nl.

process_choice(_) :-
    write('❌ Choix invalide. Veuillez choisir entre 1 et 6.~n'), nl.

generate_complete_report :-
    forall(suspect(Suspect),
           forall(crime_type(Crime),
                  (format('~w pour ~w: ', [Suspect, Crime]),
                   (is_guilty(Suspect, Crime) ->
                       write('COUPABLE')
                   ;
                       write('INNOCENT')
                   ),
                   nl))).