:- module(api, [start_server/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).
:- use_module(logique).

:- http_handler(root(enquete), enquete_handler, []).

start_server :-
    http_server(http_dispatch, [port(8182)]).

enquete_handler(Request) :-
    set_cors_headers,
    (   member(method(options), Request)
    ->  format('~n')
    ;   http_read_json_dict(Request, Dict),
        CrimeType0 = Dict.get(crimeType),
        map_crime_type(CrimeType0, CrimeType),
        Suspects = Dict.get(suspects),
        maplist(suspect_result(CrimeType), Suspects, Results),
        reply_json_dict(_{crimeType: CrimeType0, results: Results})
    ).

suspect_result(CrimeType, SuspectDict, ResultDict) :-
    dict_pairs(SuspectDict, _, Pairs),
    % verdict minimal
    check_verdict(CrimeType, Pairs, IsGuilty),
    % probabilité calculée sur tous les attributs
    check_prob(CrimeType, Pairs, Prob),
    ResultDict = SuspectDict.put(_{isGuilty: IsGuilty, probabilite: Prob, typeCrime: CrimeType}).

% --- Dispatcher verdict ---
check_verdict("assassinat", Suspect, IsGuilty) :-
    logique:est_assassin(Suspect, IsGuilty).
check_verdict("meurtre", Suspect, IsGuilty) :-
    logique:est_assassin(Suspect, IsGuilty).
check_verdict("vol", Suspect, IsGuilty) :-
    logique:est_voleur(Suspect, IsGuilty).
check_verdict("viol", Suspect, IsGuilty) :-
    logique:est_violeur(Suspect, IsGuilty).
check_verdict("escroquerie", Suspect, IsGuilty) :-
    logique:est_escroc(Suspect, IsGuilty).
check_verdict(_, _, false).

% --- Dispatcher probabilité ---
check_prob("assassinat", Suspect, Prob) :-
    logique:proba_assassin(Suspect, Prob).
check_prob("meurtre", Suspect, Prob) :-
    logique:proba_assassin(Suspect, Prob).
check_prob("vol", Suspect, Prob) :-
    logique:proba_voleur(Suspect, Prob).
check_prob("viol", Suspect, Prob) :-
    logique:proba_violeur(Suspect, Prob).
check_prob("escroquerie", Suspect, Prob) :-
    logique:proba_escroc(Suspect, Prob).
check_prob(_, _, 0).

% --- CORS ---
set_cors_headers :-
    format('Access-Control-Allow-Origin: *~n'),
    format('Access-Control-Allow-Methods: GET, POST, OPTIONS~n'),
    format('Access-Control-Allow-Headers: Content-Type~n').

% --- Mapping JSON → logique ---
map_crime_type("meurtre", "assassinat").
map_crime_type(C, C).
