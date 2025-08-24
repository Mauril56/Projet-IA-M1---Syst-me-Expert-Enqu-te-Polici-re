% Tests pour les fonctionnalités IA
:- begin_tests(ia_advanced).

test(explanation_works) :-
    explain_verdict(mary, assassinat),
    explain_verdict(john, vol).

test(certainty_calculation) :-
    certainty_level(mary, assassinat, C),
    C > 0.5.

test(hypothetical_reasoning) :-
    hypothetical_reasoning(bruno, escroquerie, has_motive(bruno, escroquerie)).

test(proof_tree) :-
    proof_tree(mary, assassinat).

test(conflict_resolution) :-
    conflict_resolution(mary).

:- end_tests(ia_advanced).