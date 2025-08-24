% Fonctions utilitaires
:- module(helpers, [
    display_list/1,
    sleep/1,
    clear_screen/0,
    press_enter_to_continue/0
]).

display_list([]).
display_list([H|T]) :-
    format('  - ~w~n', [H]),
    display_list(T).

sleep(Seconds) :-
    get_time(Now),
    Finish is Now + Seconds,
    repeat,
    get_time(Current),
    (Current >= Finish -> ! ; true).

clear_screen :-
    write('\033[2J\033[H').

press_enter_to_continue :-
    write('Appuyez sur Entrée pour continuer...'),
    read_line(_),
    clear_screen.