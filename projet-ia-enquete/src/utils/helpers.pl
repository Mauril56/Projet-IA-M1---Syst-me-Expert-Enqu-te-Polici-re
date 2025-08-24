% Fonctions utilitaires
:- module(helpers, [
    display_list/1,
    sleep/1,
    clear_screen/0,
    press_enter_to_continue/0,
    repeat_char/2,
    format_percentage/2,
    current_timestamp/1
]).

display_list([]) :- !.
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
    get_char(_),  % Lire le caractère
    skip(10).     % Ignorer jusqu'au retour à la ligne

% Répéter un caractère N fois
repeat_char(_, 0) :- !.
repeat_char(Char, N) :-
    N > 0,
    write(Char),
    N1 is N - 1,
    repeat_char(Char, N1).

% Formater un pourcentage
format_percentage(Value, FormattedPercentage) :-
    Percentage is Value * 100,
    format(atom(FormattedPercentage), '~2f%', [Percentage]).

% Obtenir un timestamp actuel
current_timestamp(Timestamp) :-
    get_time(Time),
    format_time(atom(Timestamp), '%Y-%m-%d %H:%M:%S', Time).