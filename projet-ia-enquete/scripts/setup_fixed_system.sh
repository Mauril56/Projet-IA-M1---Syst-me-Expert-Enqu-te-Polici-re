#!/bin/bash
# Script: setup_fixed_system.sh
# Description: Configuration complète du système d'enquête IA
# Version: 2.0 - Corrections et améliorations

echo "🚀 Configuration du Système d'Enquête IA Avancé..."
echo "=================================================="

# Couleurs pour l'affichage
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Fonction d'affichage coloré
print_status() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

# Vérification des dépendances
echo ""
print_info "Vérification des dépendances système..."

# Vérification de SWI-Prolog
if ! command -v swipl &> /dev/null; then
    print_error "SWI-Prolog n'est pas installé"
    print_info "Installation automatique..."
    
    # Détection du système d'exploitation
    if command -v apt-get &> /dev/null; then
        sudo apt-get update
        sudo apt-get install -y swi-prolog
    elif command -v yum &> /dev/null; then
        sudo yum install -y swi-prolog
    elif command -v dnf &> /dev/null; then
        sudo dnf install -y swi-prolog
    elif command -v pacman &> /dev/null; then
        sudo pacman -S swi-prolog
    else
        print_error "Gestionnaire de paquets non reconnu"
        print_info "Installez manuellement SWI-Prolog: https://www.swi-prolog.org/Download.html"
        exit 1
    fi
    
    # Vérification après installation
    if ! command -v swipl &> /dev/null; then
        print_error "Échec de l'installation de SWI-Prolog"
        exit 1
    fi
fi

print_status "SWI-Prolog détecté - Version: $(swipl --version 2>/dev/null | head -1 || echo 'Version inconnue')"

# Création de la structure de base si elle n'existe pas
if [ ! -d "src" ]; then
    print_info "Création de la structure de base..."
    mkdir -p {src/{core,interface,utils},web,tests,scripts,docs}
fi

# Configuration des fichiers de base
print_info "Configuration des fichiers de base..."

# Fichier crimes.pl
cat > src/core/crimes.pl << 'EOF'
% Base de données des crimes
% Fichier: crimes.pl

% Définition des crimes avec gravité
crime(vol, 'Vol avec effraction', 3).
crime(meurtre, 'Homicide volontaire', 10).
crime(fraude, 'Fraude financière', 5).
crime(agression, 'Agression physique', 7).
crime(cambriolage, 'Cambriolage résidentiel', 4).
crime(escroquerie, 'Escroquerie en ligne', 6).

% Types d'armes utilisées
arme(couteau, 'Arme blanche').
arme(pistolet, 'Arme à feu').
arme(poison, 'Substance toxique').
arme(objet_contondant, 'Objet lourd').

% Lieux de crimes
lieu_crime(bureau, 'Bureau direction').
lieu_crime(parking, 'Parking souterrain').
lieu_crime(domicile, 'Domicile victime').
lieu_crime(banque, 'Agence bancaire').
EOF

# Fichier suspects.pl
cat > src/core/suspects.pl << 'EOF'
% Base de données des suspects
% Fichier: suspects.pl

% Format: suspect(Nom, Sexe, Age, Profession, Statut)
suspect('Jean Dupont', homme, 35, 'Comptable', 'actif').
suspect('Marie Martin', femme, 28, 'Secrétaire', 'actif').
suspect('Paul Durand', homme, 42, 'Directeur', 'suspect_principal').
suspect('Sophie Bernard', femme, 31, 'Avocate', 'actif').
suspect('Michel Rousseau', homme, 45, 'Gardien', 'actif').
suspect('Claire Dubois', femme, 39, 'Infirmière', 'actif').

% Historique des suspects
historique_violence('Paul Durand').
dettes('Jean Dupont').
mensonges_detectes('Marie Martin').
antecedents_judiciaires('Michel Rousseau').

% Relations entre suspects
relation('Jean Dupont', 'Marie Martin', 'collègues').
relation('Paul Durand', 'Jean Dupont', 'supérieur').
relation('Sophie Bernard', 'Paul Durand', 'amis').
relation('Michel Rousseau', 'Claire Dubois', 'voisins').
EOF

# Fichier regles.pl
cat > src/core/regles.pl << 'EOF'
% Règles d'inférence pour l'enquête
% Fichier: regles.pl

% Règle principale de culpabilité probable
coupable_probable(Suspect) :-
    suspect(Suspect, _, _, _, _),
    mobile(Suspect, _),
    opportunite(Suspect),
    \+ alibi_solide(Suspect).

% Règles de mobile
mobile(Suspect, argent) :-
    dettes(Suspect),
    acces_finance(Suspect).

mobile(Suspect, vengeance) :-
    conflit_personnel(Suspect, _).

mobile(Suspect, passion) :-
    relation_amoureuse(Suspect, _).

% Règles d'opportunité
opportunite(Suspect) :-
    presence_scene(Suspect, _),
    acces_lieu(Suspect).

% Règles d'alibi
alibi_solide(Suspect) :-
    alibi_verifie(Suspect),
    temoin_credible(Suspect, _).

% Règles de preuves physiques
preuve_physique(Suspect) :-
    empreintes(Suspect, _).

preuve_physique(Suspect) :-
    adn_trouve(Suspect, _).

% Règles comportementales
comportement_suspect(Suspect) :-
    mensonges_detectes(Suspect).

comportement_suspect(Suspect) :-
    fuite_interrogatoire(Suspect).

% Accès aux ressources
acces_finance(Suspect) :-
    suspect(Suspect, _, _, 'Comptable', _).

acces_finance(Suspect) :-
    suspect(Suspect, _, _, 'Directeur', _).

acces_lieu(Suspect) :-
    employe_entreprise(Suspect).

acces_arme(Suspect) :-
    suspect(Suspect, _, _, 'Gardien', _).

% Liens entre suspects et lieux
employe_entreprise('Jean Dupont').
employe_entreprise('Marie Martin').
employe_entreprise('Paul Durand').
EOF

# Correction du fichier ia_advanced.pl
print_info "Configuration des fonctionnalités IA avancées..."

cat > src/core/ia_advanced.pl << 'EOF'
% 🧠 Fonctionnalités IA Avancées pour Système d'Enquête
% Fichier: ia_advanced.pl
% Version: 2.0 - Corrigé et optimisé

:- dynamic(cas_precedent/3).
:- dynamic(pattern_crime/2).
:- dynamic(conflit_resolution/2).
:- dynamic(apprentissage_actif/2).

% ========================================
% 🔍 SYSTÈME D'EXPLICATION INTELLIGENTE
% ========================================

% Génère une explication détaillée pour une conclusion
expliquer_conclusion(Suspect, Explication) :-
    findall(Raison, raison_suspicion(Suspect, Raison), Raisons),
    (Raisons = [] -> 
        Explication = 'Aucune raison de suspicion détectée' ;
        atomic_list_concat(Raisons, ', ', ExplicationStr),
        format(atom(Explication), 'Suspect ~w: ~w', [Suspect, ExplicationStr])).

% Raisons spécifiques de suspicion
raison_suspicion(Suspect, 'présence sur la scène de crime') :-
    presence_scene(Suspect, _).

raison_suspicion(Suspect, 'mobile financier détecté') :-
    mobile(Suspect, argent).

raison_suspicion(Suspect, 'alibi non vérifié') :-
    \+ alibi_verifie(Suspect).

raison_suspicion(Suspect, 'comportement suspect observé') :-
    comportement_suspect(Suspect).

raison_suspicion(Suspect, 'accès aux armes confirmé') :-
    acces_arme(Suspect).

raison_suspicion(Suspect, 'historique de violence') :-
    historique_violence(Suspect).

% ========================================
% 📊 LOGIQUE FLOUE - CALCUL DE CERTITUDE
% ========================================

% Calcule un pourcentage de certitude (0-100%)
certitude_coupable(Suspect, Pourcentage) :-
    findall(Point, point_suspicion(Suspect, Point), Points),
    sum_list(Points, Total),
    max_points(MaxPoints),
    Pourcentage is min(100, max(0, (Total * 100) // MaxPoints)).

% Attribution de points selon les preuves
point_suspicion(Suspect, 30) :- presence_scene(Suspect, _).
point_suspicion(Suspect, 25) :- mobile(Suspect, _).
point_suspicion(Suspect, 20) :- \+ alibi_verifie(Suspect).
point_suspicion(Suspect, 15) :- comportement_suspect(Suspect).
point_suspicion(Suspect, 15) :- acces_arme(Suspect).
point_suspicion(Suspect, 20) :- historique_violence(Suspect).
point_suspicion(Suspect, 10) :- empreintes(Suspect, _).
point_suspicion(Suspect, 25) :- adn_trouve(Suspect, _).

% Points maximum possible
max_points(160).

% Niveau de certitude en texte
niveau_certitude(Pourcentage, 'TRÈS ÉLEVÉ') :- Pourcentage >= 80.
niveau_certitude(Pourcentage, 'ÉLEVÉ') :- Pourcentage >= 60, Pourcentage < 80.
niveau_certitude(Pourcentage, 'MOYEN') :- Pourcentage >= 40, Pourcentage < 60.
niveau_certitude(Pourcentage, 'FAIBLE') :- Pourcentage >= 20, Pourcentage < 40.
niveau_certitude(Pourcentage, 'TRÈS FAIBLE') :- Pourcentage < 20.

% ========================================
% 🤔 RAISONNEMENT HYPOTHÉTIQUE
% ========================================

% Test "what-if" avec ajout temporaire de preuves
tester_hypothese(Suspect, NouvellePreuve, Resultat) :-
    % Sauvegarde de l'état actuel
    certitude_coupable(Suspect, CertitudeAvant),
    
    % Ajout temporaire de la preuve
    assertz(NouvellePreuve),
    
    % Nouveau calcul
    certitude_coupable(Suspect, CertitudeApres),
    
    % Nettoyage
    retract(NouvellePreuve),
    
    % Résultat
    Diff is CertitudeApres - CertitudeAvant,
    format(atom(Resultat), 
           'Impact: ~w% (avant: ~w%, après: ~w%)', 
           [Diff, CertitudeAvant, CertitudeApres]).

% Simulation de scénarios multiples
simuler_scenarios(Suspect, Scenarios, Resultats) :-
    findall(scenario(Preuve, Impact), 
            (member(Preuve, Scenarios),
             tester_hypothese(Suspect, Preuve, Impact)), 
            Resultats).

% ========================================
% 🌳 ARBRE DE PREUVES HIÉRARCHIQUE
% ========================================

% Génère un arbre de preuves pour un suspect
arbre_preuves(Suspect, Arbre) :-
    findall(preuve(Type, Detail), preuve_contre(Suspect, Type, Detail), Preuves),
    organiser_arbre(Suspect, Preuves, Arbre).

% Organisation hiérarchique des preuves
organiser_arbre(Suspect, Preuves, arbre(Suspect, Branches)) :-
    suspect(Suspect, _, _, _, _),
    findall(branche(Type, Details), 
            (setof(Type, member(preuve(Type, _), Preuves), Types),
             member(Type, Types),
             findall(Detail, member(preuve(Type, Detail), Preuves), Details)),
            Branches).

% Types de preuves contre un suspect
preuve_contre(Suspect, physique, 'Empreintes digitales') :-
    empreintes(Suspect, _).

preuve_contre(Suspect, physique, 'ADN trouvé') :-
    adn_trouve(Suspect, _).

preuve_contre(Suspect, circumstancielle, 'Présence sur les lieux') :-
    presence_scene(Suspect, _).

preuve_contre(Suspect, mobile, 'Motivation financière') :-
    mobile(Suspect, argent).

preuve_contre(Suspect, mobile, 'Motivation vengeance') :-
    mobile(Suspect, vengeance).

preuve_contre(Suspect, comportementale, 'Mensonges détectés') :-
    mensonges_detectes(Suspect).

preuve_contre(Suspect, comportementale, 'Comportement fuyant') :-
    comportement_suspect(Suspect).

% ========================================
% 🤖 APPRENTISSAGE AUTOMATIQUE SIMPLE
% ========================================

% Apprend des cas précédents
apprendre_cas(Crime, Suspect, Coupable) :-
    assertz(cas_precedent(Crime, Suspect, Coupable)),
    mettre_a_jour_patterns(Crime, Coupable),
    assertz(apprentissage_actif(Crime, Suspect)).

% Mise à jour des patterns détectés
mettre_a_jour_patterns(Crime, Coupable) :-
    retractall(pattern_crime(Crime, _)),
    findall(Resultat, cas_precedent(Crime, _, Resultat), Resultats),
    length(Resultats, Total),
    (Total > 0 ->
        (include(==(true), Resultats, Positifs),
         length(Positifs, NbPositifs),
         Probabilite is NbPositifs / Total,
         assertz(pattern_crime(Crime, Probabilite))) ;
        assertz(pattern_crime(Crime, 0.5))).

% Prédiction basée sur l'apprentissage
predire_culpabilite(Suspect, Crime, Prediction) :-
    (pattern_crime(Crime, Probabilite) ->
        (Probabilite > 0.6 -> 
            Prediction = 'Probablement coupable' ; 
            Prediction = 'Probablement innocent') ;
        Prediction = 'Données insuffisantes').

% Recommandations basées sur l'apprentissage
recommandations_enquete(Suspect, Recommandations) :-
    findall(Rec, recommandation_individuelle(Suspect, Rec), Recommandations).

recommandation_individuelle(Suspect, 'Vérifier alibi en priorité') :-
    \+ alibi_verifie(Suspect),
    certitude_coupable(Suspect, Cert),
    Cert > 50.

recommandation_individuelle(Suspect, 'Analyser motivations financières') :-
    mobile(Suspect, argent).

recommandation_individuelle(Suspect, 'Surveillance comportementale') :-
    comportement_suspect(Suspect).

% ========================================
% ⚡ RÉSOLUTION DE CONFLITS
% ========================================

% Résout les accusations multiples
resoudre_conflits(ListeSuspects, SuspectFinal) :-
    findall(certitude(Suspect, Cert), 
            (member(Suspect, ListeSuspects), 
             certitude_coupable(Suspect, Cert)), 
            Certitudes),
    sort(2, @>=, Certitudes, CertitudesTri),
    (CertitudesTri = [certitude(SuspectFinal, _)|_] ->
        true ;
        SuspectFinal = 'aucun').

% Gestion des contradictions
detecter_contradiction(Fait1, Fait2, Contradiction) :-
    contradictoire(Fait1, Fait2),
    format(atom(Contradiction), 'Contradiction détectée: ~w vs ~w', [Fait1, Fait2]).

% Définition de faits contradictoires
contradictoire(presence_scene(Suspect, Lieu1), alibi(Suspect, Lieu2, Heure)) :-
    Lieu1 \= Lieu2.

contradictoire(alibi_verifie(Suspect), mensonges_detectes(Suspect)).

% ========================================
% 🎯 ANALYSE COMPORTEMENTALE AVANCÉE
% ========================================

% Profil psychologique du suspect
profil_psychologique(Suspect, Profil) :-
    findall(Trait, trait_psychologique(Suspect, Trait), Traits),
    (Traits = [] ->
        Profil = 'Profil comportemental normal' ;
        atomic_list_concat(Traits, ', ', Profil)).

% Traits psychologiques déduits
trait_psychologique(Suspect, 'Tendances agressives') :-
    historique_violence(Suspect).

trait_psychologique(Suspect, 'Instabilité financière') :-
    mobile(Suspect, argent),
    dettes(Suspect).

trait_psychologique(Suspect, 'Comportement fuyant') :-
    \+ alibi_verifie(Suspect),
    mensonges_detectes(Suspect).

trait_psychologique(Suspect, 'Personnalité manipulatrice') :-
    mensonges_detectes(Suspect),
    relation(Suspect, _, _).

% ========================================
% 📈 GÉNÉRATION DE RAPPORTS IA
% ========================================

% Génère un rapport complet d'analyse IA
generer_rapport_ia(Suspect, Rapport) :-
    certitude_coupable(Suspect, Certitude),
    niveau_certitude(Certitude, Niveau),
    expliquer_conclusion(Suspect, Explication),
    profil_psychologique(Suspect, Profil),
    recommandations_enquete(Suspect, Recs),
    atomic_list_concat(Recs, ', ', RecsStr),
    format(atom(Rapport),
           '=== RAPPORT IA AVANCÉ ===~n' +
           'Suspect: ~w~n' +
           'Certitude: ~w% (~w)~n' +
           'Analyse: ~w~n' +
           'Profil: ~w~n' +
           'Recommandations: ~w~n' +
           '========================~n',
           [Suspect, Certitude, Niveau, Explication, Profil, RecsStr]).

% ========================================
% 🔧 UTILITAIRES IA
% ========================================

% Initialisation du système IA
initialiser_ia :-
    write('🧠 Initialisation du système IA...'), nl,
    % Chargement des cas d'exemple
    assertz(cas_precedent(vol, 'Jean Dupont', true)),
    assertz(cas_precedent(meurtre, 'Marie Martin', false)),
    assertz(cas_precedent(fraude, 'Paul Durand', true)),
    % Calcul des patterns initiaux
    mettre_a_jour_patterns(vol, true),
    mettre_a_jour_patterns(meurtre, false),
    mettre_a_jour_patterns(fraude, true),
    write('✅ Système IA initialisé avec succès'), nl.

% Test complet de tous les modules IA
tester_modules_ia :-
    write('🧪 Test des modules IA...'), nl,
    
    % Test logique floue
    (certitude_coupable('Jean Dupont', Cert1) ->
        format('✅ Certitude Jean Dupont: ~w%~n', [Cert1]) ;
        write('❌ Erreur calcul certitude')), nl,
    
    % Test explication
    (expliquer_conclusion('Jean Dupont', Exp1) ->
        format('✅ Explication: ~w~n', [Exp1]) ;
        write('❌ Erreur génération explication')), nl,
    
    % Test apprentissage
    (apprendre_cas(test, 'Test Suspect', true) ->
        write('✅ Apprentissage: OK') ;
        write('❌ Erreur apprentissage')), nl,
    
    % Test prédiction
    (predire_culpabilite('Nouveau Suspect', vol, Pred) ->
        format('✅ Prédiction: ~w~n', [Pred]) ;
        write('❌ Erreur prédiction')), nl,
    
    write('✅ Tous les modules IA testés'), nl.

% Statistiques du système
statistiques_systeme :-
    findall(S, suspect(S, _, _, _, _), Suspects),
    length(Suspects, NbSuspects),
    findall(C, cas_precedent(C, _, _), Cas),
    length(Cas, NbCas),
    format('📊 Suspects chargés: ~w~n', [NbSuspects]),
    format('📊 Cas d\'apprentissage: ~w~n', [NbCas]).
EOF

print_status "Fichier ia_advanced.pl corrigé et optimisé"

# Correction du fichier principal enquete_core.pl
print_info "Configuration du module principal..."

cat > src/core/enquete_core.pl << 'EOF'
% Point d'entrée principal du système d'enquête IA
% Fichier: enquete_core.pl
% Version: 2.0 - Système complet et corrigé

% Chargement des modules
:- consult('crimes.pl').
:- consult('suspects.pl').
:- consult('regles.pl').
:- consult('ia_advanced.pl').
:- consult('../utils/helpers.pl').

% Initialisation globale du système
:- initialization(init_system).

% ========================================
% 🚀 INITIALISATION SYSTÈME
% ========================================

init_system :-
    write('🔍 Initialisation du Système d\'Enquête IA...'), nl,
    charger_donnees_base,
    initialiser_ia,
    write('✅ Système prêt à fonctionner'), nl.

% Chargement des données de base
charger_donnees_base :-
    % Chargement automatique des faits de base pour les tests
    assertz(presence_scene('Jean Dupont', 'bureau')),
    assertz(mobile('Jean Dupont', argent)),
    assertz(alibi_verifie('Marie Martin')),
    assertz(comportement_suspect('Paul Durand')),
    assertz(empreintes('Jean Dupont', 'coffre-fort')),
    assertz(acces_arme('Paul Durand')),
    assertz(adn_trouve('Paul Durand', 'couteau')),
    assertz(presence_scene('Michel Rousseau', 'parking')),
    write('📊 Données de test chargées'), nl.

% ========================================
% 🎯 INTERFACE PRINCIPALE
% ========================================

% Point d'entrée principal
start_enquete :-
    init_system,
    afficher_titre,
    menu_principal.

% Affichage du titre
afficher_titre :-
    nl,
    write('██████████████████████████████████████████'), nl,
    write('██    🕵️  SYSTÈME D\'ENQUÊTE IA AVANCÉ   ██'), nl,
    write('██████████████████████████████████████████'), nl,
    write('██  Intelligence Artificielle Symbolique ██'), nl,
    write('██████████████████████████████████████████'), nl,
    nl.

% Menu principal interactif
menu_principal :-
    repeat,
    afficher_menu,
    read_line_to_codes(user_input, Codes),
    (Codes = [] -> Choix = 0 ; (number_codes(Choix, Codes) -> true ; Choix = 0)),
    traiter_choix(Choix),
    Choix = 9, !.

% Affichage du menu
afficher_menu :-
    nl,
    write('┌─────────────────────────────────────┐'), nl,
    write('│           🎯 MENU PRINCIPAL          │'), nl,
    write('├─────────────────────────────────────┤'), nl,
    write('│ 1. 📋 Lister les suspects          │'), nl,
    write('│ 2. 🔍 Analyser un suspect          │'), nl,
    write('│ 3. 🧠 Analyse IA avancée            │'), nl,
    write('│ 4. 📊 Rapport de certitude          │'), nl,
    write('│ 5. 🤔 Test d\'hypothèse              │'), nl,
    write('│ 6. 🌳 Arbre de preuves              │'), nl,
    write('│ 7. 🤖 Apprentissage automatique     │'), nl,
    write('│ 8. 🧪 Tests système                 │'), nl,
    write('│ 9. 🚪 Quitter                       │'), nl,
    write('└─────────────────────────────────────┘'), nl,
    write('Votre choix (1-9): ').

% Traitement des choix du menu
traiter_choix(1) :- lister_suspects.
traiter_choix(2) :- analyser_suspect_interactif.
traiter_choix(3) :- analyse_ia_complete.
traiter_choix(4) :- rapport_certitude_tous.
traiter_choix(5) :- test_hypothese_interactif.
traiter_choix(6) :- afficher_arbre_preuves.
traiter_choix(7) :- apprentissage_interactif.
traiter_choix(8) :- tester_modules_ia.
traiter_choix(9) :- 
    nl, write('👋 Au revoir! Enquête terminée.'), nl.
traiter_choix(_) :- 
    write('❌ Choix invalide. Réessayez.'), nl, fail.

% ========================================
% 📋 FONCTIONS DU MENU
% ========================================

% 1. Lister tous les suspects
lister_suspects :-
    nl, write('📋 LISTE DES SUSPECTS:'), nl,
    write('═══════════════════════'), nl,
    forall(suspect(Nom, Sexe, Age, Profession, Statut),
           (certitude_coupable(Nom, Cert),
            format('👤 ~w (~w, ~w ans) - ~w [~w] - Certitude: ~w%~n', 
                   [Nom, Sexe, Age, Profession, Statut, Cert]))).

% 2. Analyser un suspect spécifique
analyser_suspect_interactif :-
    nl, write('🔍 Entrez le nom du suspect à analyser: '),
    read_line_to_codes(user_input, Codes),
    atom_codes(SuspectAtom, Codes),
    (suspect(SuspectAtom, _, _, _, _) ->
        analyser_suspect_complet(SuspectAtom) ;
        write('❌ Suspect non trouvé.')), nl.

analyser_suspect_complet(Suspect) :-
    nl, format('🔍 ANALYSE COMPLÈTE: ~w~n', [Suspect]),
    write('═══════════════════════════════'), nl,
    
    % Informations de base
    suspect(Suspect, Sexe, Age, Profession, Statut),
    format('Profil: ~w (~w, ~w ans) - Statut: ~w~n', [Profession, Sexe, Age, Statut]),
    
    % Analyse de culpabilité
    (coupable_probable(Suspect) ->
        write('⚠️  Statut: SUSPECT PRINCIPAL') ;
        write('✅ Statut: Peu suspect')), nl,
    
    % Certitude IA
    certitude_coupable(Suspect, Cert),
    niveau_certitude(Cert, Niveau),
    format('🤖 Certitude IA: ~w% (~w)~n', [Cert, Niveau]),
    
    % Explication
    expliquer_conclusion(Suspect, Explication),
    format('💡 Analyse: ~w~n', [Explication]),
    
    % Profil psychologique
    profil_psychologique(Suspect, Profil),
    format('🧠 Profil: ~w~n', [Profil]).

% 3. Analyse IA complète de tous les suspects
analyse_ia_complete :-
    nl, write('🧠 ANALYSE IA AVANCÉE - TOUS SUSPECTS'), nl,
    write('══════════════════════════════════════'), nl,
    forall(suspect(Nom, _, _, _, _),
           (generer_rapport_ia(Nom, Rapport),
            write(Rapport), nl)).

% 4. Rapport de certitude pour tous
rapport_certitude_tous :-
    nl, write('📊 RAPPORT DE CERTITUDE GLOBALE'), nl,
    write('═══════════════════════════════════'), nl,
    findall(cert(Nom, Cert), 
                        (suspect(Nom, _, _, _, _), certitude_coupable(Nom, Cert)), 
            Certs),
    forall(member(cert(Nom, Cert), Certs),
           format('👤 ~w -> Certitude: ~w%~n', [Nom, Cert]))).

% 5. Test d'hypothèse interactif
test_hypothese_interactif :-
    nl, write('🤔 Entrez le nom du suspect: '),
    read_line_to_codes(user_input, Codes1),
    atom_codes(Suspect, Codes1),
    write('Ajoutez une preuve hypothétique (ex: empreintes(\'Suspect\', \'objet\')): '), nl,
    read(Term),
    (tester_hypothese(Suspect, Term, Resultat) ->
        format('✅ Résultat: ~w~n', [Resultat]) ;
        write('❌ Erreur dans le test d\'hypothèse')), nl.

% 6. Affichage arbre de preuves
afficher_arbre_preuves :-
    nl, write('🌳 ARBRE DE PREUVES - Entrez le nom du suspect: '),
    read_line_to_codes(user_input, Codes),
    atom_codes(Suspect, Codes),
    (arbre_preuves(Suspect, Arbre) ->
        portray_clause(Arbre) ;
        write('❌ Aucune preuve trouvée pour ce suspect')), nl.

% 7. Apprentissage interactif
apprentissage_interactif :-
    nl, write('🤖 Entrez le type de crime: '),
    read(Crime),
    write('Entrez le nom du suspect: '),
    read_line_to_codes(user_input, Codes),
    atom_codes(Suspect, Codes),
    write('Le suspect est-il coupable ? (true/false): '),
    read(Coupable),
    (apprendre_cas(Crime, Suspect, Coupable) ->
        write('✅ Cas appris avec succès') ;
        write('❌ Échec apprentissage du cas')), nl.

EOF

# Fichier utils/helpers.pl
print_info "Création du module utilitaire helpers.pl..."

cat > src/utils/helpers.pl << 'EOF'
% Outils utilitaires généraux
% Fichier: helpers.pl

% Conversion en majuscule
to_uppercase(Input, Output) :-
    atom_chars(Input, Chars),
    maplist(char_upper, Chars, UpperChars),
    atom_chars(Output, UpperChars).

char_upper(Char, Upper) :-
    char_type(Char, lower(Upper)), !.
char_upper(Char, Char).

% Ligne séparatrice
separator :-
    write('──────────────────────────────'), nl.
EOF

print_status "Fichiers core et utils configurés avec succès"

# Création d'un script de lancement
print_info "Création du script de lancement enquete.sh..."

cat > scripts/enquete.sh << 'EOF'
#!/bin/bash
# Script de lancement du système d'enquête IA

echo "🕵️  Lancement du Système d'Enquête IA..."
echo "======================================="

swipl -s src/core/enquete_core.pl -g start_enquete -t halt
EOF
chmod +x scripts/enquete.sh

print_status "Script enquete.sh prêt à l'emploi"

echo ""
print_status "✅ Configuration terminée avec succès"
print_info "Pour lancer l'enquête IA: ./scripts/enquete.sh"
