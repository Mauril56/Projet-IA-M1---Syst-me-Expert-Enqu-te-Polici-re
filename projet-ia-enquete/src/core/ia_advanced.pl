:- module(ia_advanced, [
    explain_verdict/2,
    certainty_level/3,
    hypothetical_reasoning/3,
    proof_tree/2,
    machine_learning/0,
    conflict_resolution/1
]).

:- use_module('../core/crimes.pl').
:- use_module('../core/suspects.pl').
:- use_module('../core/regles.pl').

explain_verdict(Suspect, Crime) :-
    format('~n🔍 EXPLICATION pour ~w accusé de ~w:~n', [Suspect, Crime]),
    format('=============================================~n', []),
    
    (regles:is_guilty(Suspect, Crime) ->
        format('✅ CONCLUSION: COUPABLE~n~n'),
        format('📋 PREUVES RETENUES:~n'),
        show_evidence(Suspect, Crime),
        
        % Calcul de la certitude
        certainty_level(Suspect, Crime, Certainty),
        format('~n📊 NIVEAU DE CERTITUDE: ~2f%~n', [Certainty*100])
    ;
        format('❌ CONCLUSION: INNOCENT~n~n'),
        format('📋 ABSENCE DE PREUVES:~n'),
        show_missing_evidence(Suspect, Crime),
        
        format('~n💡 RAISON: Preuves insuffisantes pour condamnation~n', [])
    ),
    format('=============================================~n~n').


show_evidence(Suspect, Crime) :-
    (suspects:has_motive(Suspect, Crime) -> 
        format('   • Motif: ~w avait une raison de commettre ce crime~n', [Suspect]) ; true),
    (suspects:was_near_crime_scene(Suspect, Crime) -> 
        format('   • Présence: ~w était sur les lieux du crime~n', [Suspect]) ; true),
    (suspects:has_fingerprint_on_weapon(Suspect, Crime) -> 
        format('   • Empreintes: Traces sur l\'arme du crime~n', []) ; true),
    (suspects:eyewitness_identification(Suspect, Crime) -> 
        format('   • Témoin: Identification par témoin oculaire~n', []) ; true),
    (suspects:has_bank_transaction(Suspect, Crime) -> 
        format('   • Transaction: Mouvement bancaire suspect~n', []) ; true),
    (suspects:owns_fake_identity(Suspect, Crime) -> 
        format('   • Identité: Possession de fausse identité~n', []) ; true).

show_missing_evidence(Suspect, Crime) :-
    (\+ suspects:has_motive(Suspect, Crime) -> 
        format('   • Aucun motif identifiable~n', []) ; true),
    (\+ suspects:was_near_crime_scene(Suspect, Crime) -> 
        format('   • Alibi: Non présent sur les lieux~n', []) ; true),
    (\+ suspects:has_fingerprint_on_weapon(Suspect, Crime) -> 
        format('   • Aucune empreinte sur arme~n', []) ; true),
    (\+ suspects:has_bank_transaction(Suspect, Crime) -> 
        format('   • Aucune transaction suspecte~n', []) ; true).


certainty_level(Suspect, Crime, Certainty) :-
    findall(Weight, proof_exists_with_weight(Suspect, Crime, Weight), Weights),
    sum_list(Weights, TotalWeight),
    Certainty is min(1.0, TotalWeight / 100).


proof_exists_with_weight(Suspect, Crime, 30) :- suspects:has_motive(Suspect, Crime).
proof_exists_with_weight(Suspect, Crime, 25) :- suspects:was_near_crime_scene(Suspect, Crime).
proof_exists_with_weight(Suspect, Crime, 35) :- suspects:has_fingerprint_on_weapon(Suspect, Crime).
proof_exists_with_weight(Suspect, Crime, 40) :- suspects:eyewitness_identification(Suspect, Crime).
proof_exists_with_weight(Suspect, Crime, 20) :- suspects:has_bank_transaction(Suspect, Crime).
proof_exists_with_weight(Suspect, Crime, 15) :- suspects:owns_fake_identity(Suspect, Crime).

hypothetical_reasoning(Suspect, Crime, AdditionalProof) :-
    format('~n🤔 SCÉNARIO HYPOTHÉTIQUE pour ~w (~w)~n', [Suspect, Crime]),
    format('Preuve supplémentaire: ~w~n~n', [AdditionalProof]),
    
    % État actuel
    (regles:is_guilty(Suspect, Crime) ->
        format('📌 ÉTAT ACTUEL: COUPABLE~n', [])
    ;
        format('📌 ÉTAT ACTUEL: INNOCENT~n', [])
    ),
    
    % Ajout temporaire de la preuve et réévaluation
    assertz(AdditionalProof),
    
    (regles:is_guilty(Suspect, Crime) ->
        format('📌 AVEC NOUVELLE PREUVE: COUPABLE~n', [])
    ;
        format('📌 AVEC NOUVELLE PREUVE: INNOCENT~n', [])
    ),
    
    % Retrait de la preuve temporaire
    retract(AdditionalProof),
    format('~n💡 Preuve temporaire retirée~n', []).

:- dynamic learned_pattern/3.
:- dynamic case_history/4.

machine_learning :-
    retractall(learned_pattern(_, _, _)),
    retractall(case_history(_, _, _, _)),
    
    % Analyse des patterns dans les cas résolus
    forall((suspects:suspect(S), crimes:crime_type(C), regles:is_guilty(S, C)),
           learn_from_case(S, C)),
    
    format('🤖 Système d\'apprentissage activé~n', []),
    count_learned_patterns(Count),
    format('📊 ~w patterns appris~n', [Count]).

learn_from_case(Suspect, Crime) :-
    findall(ProofType, proof_exists_for_case(Suspect, Crime, ProofType), ProofTypes),
    assertz(learned_pattern(Crime, ProofTypes, Suspect)),
    assertz(case_history(Suspect, Crime, ProofTypes, guilty)).

proof_exists_for_case(Suspect, Crime, motive) :- suspects:has_motive(Suspect, Crime).
proof_exists_for_case(Suspect, Crime, presence) :- suspects:was_near_crime_scene(Suspect, Crime).
proof_exists_for_case(Suspect, Crime, fingerprints) :- suspects:has_fingerprint_on_weapon(Suspect, Crime).
proof_exists_for_case(Suspect, Crime, witness) :- suspects:eyewitness_identification(Suspect, Crime).
proof_exists_for_case(Suspect, Crime, transaction) :- suspects:has_bank_transaction(Suspect, Crime).
proof_exists_for_case(Suspect, Crime, fake_id) :- suspects:owns_fake_identity(Suspect, Crime).

count_learned_patterns(Count) :-
    findall(P, learned_pattern(_, _, P), Patterns),
    length(Patterns, Count).

proof_tree(Suspect, Crime) :-
    format('~n🌳 ARBRE DE PREUVES pour ~w (~w)~n', [Suspect, Crime]),
    format('=============================================~n', []),
    display_proof_tree(Suspect, Crime, 0),
    format('=============================================~n~n').

display_proof_tree(Suspect, Crime, Depth) :-
    indent(Depth),
    format('├── Suspect: ~w - Crime: ~w~n', [Suspect, Crime]),
    NewDepth is Depth + 1,
    
    % Afficher chaque type de preuve
    check_and_display_proof(Suspect, Crime, motive, 'Motif', 30, NewDepth),
    check_and_display_proof(Suspect, Crime, presence, 'Présence', 25, NewDepth),
    check_and_display_proof(Suspect, Crime, fingerprints, 'Empreintes', 35, NewDepth),
    check_and_display_proof(Suspect, Crime, witness, 'Témoin', 40, NewDepth),
    check_and_display_proof(Suspect, Crime, transaction, 'Transaction', 20, NewDepth),
    check_and_display_proof(Suspect, Crime, fake_id, 'Fausse ID', 15, NewDepth).

check_and_display_proof(Suspect, Crime, ProofType, ProofName, Weight, Depth) :-
    indent(Depth),
    (proof_exists_for_case(Suspect, Crime, ProofType) ->
        format('├── ✅ ~w (Poids: ~d)~n', [ProofName, Weight])
    ;
        format('├── ❌ ~w (Absent)~n', [ProofName])
    ).

indent(0) :- !.
indent(Depth) :-
    Depth > 0,
    forall(between(1, Depth, _), write('│   ')).


conflict_resolution(Suspect) :-
    findall(Crime, (crimes:crime_type(Crime), regles:is_guilty(Suspect, Crime)), Crimes),
    length(Crimes, Count),
    (Count > 1 ->
        format('⚡ CONFLIT: ~w accusé de ~d crimes~n', [Suspect, Count]),
        format('📋 Crimes: ~w~n', [Crimes]),
        resolve_conflicts(Suspect, Crimes)
    ; Count =:= 1 ->
        Crimes = [OneCrime],
        format('✅ ~w accusé uniquement de: ~w~n', [Suspect, OneCrime])
    ;
        format('✅ ~w n\'est accusé d\'aucun crime~n', [Suspect])
    ).

resolve_conflicts(Suspect, Crimes) :-
    findall(Certainty-Crime, 
            (member(Crime, Crimes), certainty_level(Suspect, Crime, Certainty)), 
            Certainties),
    sort(Certainties, Sorted),
    reverse(Sorted, [BestCertainty-BestCrime|_]),
    format('🎯 Crime principal: ~w (Certitude: ~2f%)~n', [BestCrime, BestCertainty*100]).