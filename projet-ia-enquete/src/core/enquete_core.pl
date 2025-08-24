% Module principal mis à jour
:- module(enquete_core, [
    main/0
]).

:- use_module('../core/crimes.pl').
:- use_module('../core/suspects.pl').
:- use_module('../core/regles.pl').
:- use_module('../core/ia_advanced.pl').
:- use_module('../utils/helpers.pl').
:- use_module('../utils/validations.pl').
:- use_module('../interface/cli_interface.pl').

% Point d'entrée principal avec IA
main :- 
    write('================================='), nl,
    write('  SYSTÈME EXPERT IA - ENQUÊTE'), nl,
    write('  avec fonctionnalités avancées'), nl,
    write('================================='), nl,
    ia_advanced:machine_learning,
    cli_interface:start_cli.