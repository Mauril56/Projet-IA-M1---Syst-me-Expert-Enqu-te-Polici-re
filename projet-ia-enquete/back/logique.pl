:- module(logique, [
    est_assassin/2,
    est_voleur/2,
    est_violeur/2,
    est_escroc/2,
    proba_assassin/2,
    proba_voleur/2,
    proba_violeur/2,
    proba_escroc/2
]).

% ---------- ASSASSIN ----------
est_assassin(Suspect, true) :-
    member(arme-"oui", Suspect),
    ( member(distance-"pret", Suspect)
    ; member(empreinteLieu-"oui", Suspect)
    ; member(empreinteArme-"oui", Suspect)
    ), !.
est_assassin(_, false).

proba_assassin(Suspect, Prob) :-
    % Poids des attributs
    ( member(arme-"oui", Suspect) -> A=30 ; A=5 ),
    ( member(distance-"pret", Suspect) -> D=20 ; D=5 ),
    ( member(empreinteLieu-"oui", Suspect) -> L=20 ; L=5 ),
    ( member(empreinteArme-"oui", Suspect) -> R=10 ; R=5 ),
    ( member(relation-"ami", Suspect) -> Rel=5 ; Rel=10 ),
    ( member(sex-"homme", Suspect) -> Sx=5 ; Sx=5 ),
    ( member(age-Age, Suspect), Age > 40 -> AgeScore=5 ; AgeScore=5 ),
    Prob0 is A + D + L + R + Rel + Sx + AgeScore,
    Prob is min(Prob0, 100).

% ---------- VOL ----------

est_voleur(Suspect, true) :-
    ( member(relation-"employe", Suspect)
    ; member(relation-"ami", Suspect)
    ; member(arme-"oui", Suspect)
    ),
    member(empreinteLieu-"oui", Suspect),
    member(distance-"pret", Suspect), !.

est_voleur(_, false).


% Calcul de la probabilité en fonction de toutes les informations
proba_voleur(Suspect, Prob) :-
    % Poids des attributs
    ( member(empreinteLieu-"oui", Suspect) -> L=40 ; L=5 ),
    ( member(relation-"dette", Suspect) -> R1=10 ; R1=1 ),
    ( member(relation-"employe", Suspect) -> R2=10 ; R2=1 ),
    ( member(arme-"oui", Suspect) -> A=15 ; A=5 ),      % ajout de la présence d'une arme
    ( member(distance-"pret", Suspect) -> D=15 ; D=5 ),
    ( member(temoin-"oui", Suspect) -> T=20 ; T=0 ),
    ( member(sex-"homme", Suspect) -> Sx=5 ; Sx=4 ),
    ( member(age-Age, Suspect), Age < 30 -> AgeScore=5 ; AgeScore=2 ),

    % Somme brute
    Prob0 is L + R1 + R2 + A + D + T + Sx + AgeScore,

    % Limiter entre 0 et 100%
    Prob is min(Prob0, 100).

% ---------- VIOL ----------
est_violeur(Suspect, true) :-
    ( member(empreinteLieu-"oui", Suspect)
    , member(distance-"pret", Suspect)
    ), !.
est_violeur(_, false).

proba_violeur(Suspect, Prob) :-
    ( member(empreinteLieu-"oui", Suspect) -> A=20 ; A=5 ),
    ( member(distance-"pret", Suspect) -> V=20 ; V=5 ),
    ( member(relation-"employeur", Suspect) -> T=10 ; T=5 ),
    ( member(sex-"homme", Suspect) -> Sx=7 ; Sx=5 ),
    ( member(age-Age, Suspect), Age>20 -> AgeScore=5 ; AgeScore=3 ),
    Prob0 is A+V+T+Sx+AgeScore,
    Prob is min(Prob0, 100).

% ---------- ESCROQUERIE ----------
est_escroc(Suspect, true) :-
    ( member(empreinteLieu-"oui", Suspect)
    , member(distance-"pret", Suspect)
    
    ), !.
est_escroc(_, false).

proba_escroc(Suspect, Prob) :-
    ( member(distance-"pret", Suspect) -> F=20 ; F=5 ),
    ( member(empreinteLieu-"oui", Suspect) -> B=20 ; B=5 ),
    ( member(arme-"oui", Suspect) -> T=5 ; T=10 ),
    (member(sex-"homme", Suspect) -> Sx=5 ; Sx=4 ),
    ( member(age-Age, Suspect), Age>35 -> AgeScore=4 ; AgeScore=2 ),
    Prob0 is F+B+T+Sx+AgeScore,
    Prob is min(Prob0, 100).
