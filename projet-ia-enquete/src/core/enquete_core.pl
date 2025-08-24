:- module(enquete_core, [
    main/0
]).

:- use_module('../core/crimes.pl').
:- use_module('../core/suspects.pl').
:- use_module('../core/regles.pl').
:- use_module('../core/ia_advanced.pl').
:- use_module('../utils/helpers.pl').
:- use_module('../interface/cli_interface.pl').

main :- 
    write('================================='), nl,
    write('  SYSTÈME EXPERT IA - ENQUÊTE'), nl,
    write('================================='), nl,
    nl,
    
    % Initialisation du système d'apprentissage
    ia_advanced:machine_learning,
    nl,
    
    % Démarrage de l'interface CLI
    cli_interface:start_cli.