:- module(ar_router, [
    ar_route/1,     % +Request
    route_get/2,    % +Route, :Goal
    route_post/2,   % +Route, :Goal
    route_put/2,    % +Route, :Goal
    route_del/2,    % +Route, :Goal
    route_get/3,    % +Route, :BeforeGoal, :Goal
    route_post/3,   % +Route, :BeforeGoal, :Goal
    route_put/3,    % +Route, :BeforeGoal, :Goal
    route_del/3,    % +Route, :BeforeGoal, :Goal
    new_route/4,    % +Method, +Route, :BeforeGoal, :Goal
    path_to_route/2 % +Path, -Route
]).

/** <module> Alternative HTTP routing

HTTP routing with path expressions.
*/

:- use_module(library(debug)).

% route(?Method, ?Route, ?Before, ?Goal).

:- dynamic(route/4).

:- meta_predicate(route_get(+, 0)).
:- meta_predicate(route_post(+, 0)).
:- meta_predicate(route_put(+, 0)).
:- meta_predicate(route_del(+, 0)).
:- meta_predicate(route_get(+, 1, 0)).
:- meta_predicate(route_post(+, 1, 0)).
:- meta_predicate(route_put(+, 1, 0)).
:- meta_predicate(route_del(+, 1, 0)).
:- meta_predicate(new_route(+, +, 0)).
:- meta_predicate(new_route(+, +, 1, 0)).

%! route_get(+Route, :Goal) is det.
%
% Registers a new GET route handler.

route_get(Route, Goal):-
    new_route(get, Route, Goal).

%! route_put(+Route, :Goal) is det.
%
% Registers a new PUT route handler.

route_put(Route, Goal):-
    new_route(put, Route, Goal).

%! route_del(+Route, :Goal) is det.
%
% Registers a new DELETE route handler.
    
route_del(Route, Goal):-
    new_route(delete, Route, Goal).

%! route_post(+Route, :Goal) is det.
%
% Registers a new POST route handler.
    
route_post(Route, Goal):-
    new_route(post, Route, Goal).

%! route_get(+Route, :Before, :Goal) is det.
%
% Registers a new GET route handler.
% Accepts Before goal.

route_get(Route, Before, Goal):-
    new_route(get, Route, Before, Goal).

%! route_put(+Route, :Before, :Goal) is det.
%
% Registers a new PUT route handler.
% Accepts Before goal.
    
route_put(Route, Before, Goal):-
    new_route(put, Route, Before, Goal).
    
%! route_del(+Route, :Before, :Goal) is det.
%
% Registers a new DELETE route handler.
% Accepts Before goal.
    
route_del(Route, Before, Goal):-
    new_route(delete, Route, Before, Goal).

%! route_post(+Route, :Before, :Goal) is det.
%
% Registers a new POST route handler.
% Accepts Before goal.
    
route_post(Route, Before, Goal):-
    new_route(post, Route, Before, Goal).

%! new_route(+Method, +Route, :Before, :Goal) is det.
%
% Registers a new method-specific route handler.
% Does nothing when the route already exists
% for the method.
    
new_route(Method, Route, Before, Goal):-
    (   route(Method, Route, _, _)
    ->  true
    ;   assertz(route(Method, Route, goal(Before), Goal))).

%! new_route(+Method, +Route, :Goal) is det.
%
% Registers a new method-specific route handler.
% Does nothing when the route already exists
% for the method.

new_route(Method, Route, Goal):-
    (   route(Method, Route, _, _)
    ->  true
    ;   assertz(route(Method, Route, none, Goal))).

%! ar_route(+Request) is semidet.
%
% Routes the request into an handler
% Fails when no handler is found.
%
% Request must contain method(Method)
% and path(Path).
    
ar_route(Request):-
    memberchk(method(Method), Request),
    memberchk(path(Path), Request),
    path_to_route(Path, Route),
    debug(ar_route, 'dispatch: ~p ~p', [Method, Route]),
    dispatch(Method, Route).

%! dispatch(+Method, +Route) is semidet.
%
% Attempts to dispatch the request.
% Fails when no matching handler is found.
%
% Throws handler_failed(Method, Path) when
% handler was found but it failed during
% execution.

dispatch(Method, Path):-
    route(Method, Path, Before, Goal), !,
    (   run_handler(Before, Goal)
    ->  true
    ;   throw(error(handler_failed(Method, Path)))).
    
run_handler(Before, Goal):- !,
    (   Before = goal(BeforeGoal)
    ->  call(BeforeGoal, ar_router:run_handler(Goal))
    ;   run_handler(Goal)).

run_handler(Handler):-
    call(Handler).

%! path_to_route(+Path, -Route) is det.
%
% Turns path atom like '/path/to/something' into
% a Prolog term path/to/something.
    
path_to_route(Path, Route):-    
    atom_codes(Path, Codes),
    phrase(path_tokens([/|Tokens]), Codes),
    path_to_route_term(Tokens, Route), !.
    
path_to_route_term([], /).
    
path_to_route_term([First|Rest], Term):-
    path_to_route_term(Rest, First, Term).
    
path_to_route_term([/,A|Rest], Acc, Term):-
    path_to_route_term(Rest, /(Acc, A), Term).

path_to_route_term([A], Acc, Route):-
    (   A = (/)
    ->  Route = /(Acc, '')
    ;   Route = /(Acc, A)).

path_to_route_term([], Acc, Acc).

path_tokens([Token|Tokens]) -->
    path_token(Token),
    path_tokens(Tokens).
    
path_tokens([]) --> [].

path_token(/) --> "/", !.

path_token(Atom) -->
    path_char(Char), !,
    path_char_token(Chars),
    { atom_chars(Atom, [Char|Chars]) }.

path_char_token([Char|Chars]) -->
    path_char(Char), !,
    path_char_token(Chars).
    
path_char_token([]) --> [].

path_char(Char) --> [Char], { Char \= 0'/ }.
