:- module(ar_router, [
    ar_route/1,     % +Request
    route_get/2,    % +Route, :Goal
    route_post/2,   % +Route, :Goal
    route_put/2,    % +Route, :Goal
    route_del/2,    % +Route, :Goal
    route_get/3,    % +Route, +Before:list, :Goal
    route_post/3,   % +Route, +Before:list, :Goal
    route_put/3,    % +Route, +Before:list, :Goal
    route_del/3,    % +Route, +Before:list, :Goal
    new_route/4,    % +Method, +Route, +Before:list, :Goal
    path_to_route/2 % +Path, -Route
]).

/** <module> Alternative HTTP routing

Can be used with http_dispatch through
the fallback closure.
*/

:- use_module(library(debug)).

%% route(?Method, ?Route, ?Module, ?Goal, ?Before) is nondet.

:- dynamic(route/5).

:- module_transparent(ar_route/2).
:- module_transparent(route_get/2).
:- module_transparent(route_post/2).
:- module_transparent(route_put/2).
:- module_transparent(route_del/2).
:- module_transparent(route_get/3).
:- module_transparent(route_post/3).
:- module_transparent(route_put/3).
:- module_transparent(route_del/3).
:- module_transparent(new_route/4).

%% route_get(+Route, :Goal) is det.
%
% Registers a new GET route handler.

route_get(Route, Goal):-
    new_route(get, Route, [], Goal).

%% route_put(+Route, :Goal) is det.
%
% Registers a new PUT route handler.

route_put(Route, Goal):-
    new_route(put, Route, [], Goal).

%% route_del(+Route, :Goal) is det.
%
% Registers a new DELETE route handler.
    
route_del(Route, Goal):-
    new_route(del, Route, [], Goal).

%% route_post(+Route, :Goal) is det.
%
% Registers a new POST route handler.
    
route_post(Route, Goal):-
    new_route(post, Route, [], Goal).

%% route_get(+Route, +Before:list, :Goal) is det.
%
% Registers a new GET route handler.
% Takes list of before-handlers.

route_get(Route, Before, Goal):-
    new_route(get, Route, Before, Goal).

%% route_put(+Route, +Before:list, :Goal) is det.
%
% Registers a new PUT route handler.
% Takes list of before-handlers.
    
route_put(Route, Before, Goal):-
    new_route(put, Route, Before, Goal).
    
%% route_del(+Route, +Before:list, :Goal) is det.
%
% Registers a new DELETE route handler.
% Takes list of before-handlers.
    
route_del(Route, Before, Goal):-
    new_route(del, Route, Before, Goal).

%% route_post(+Route, +Before:list, :Goal) is det.
%
% Registers a new POST route handler.
% Takes list of before-handlers.
    
route_post(Route, Before, Goal):-
    new_route(post, Route, Before, Goal).

%% new_route(+Method, +Route, +Before:list, :Goal) is det.
%
% Registers a new method-specific route handler.
% Does nothing when the route already exists
% for the method.
    
new_route(Method, Route, Before, Goal):-
    (   ar_router:route(Method, Route, _, _, _)
    ->  true
    ;   context_module(Module),
        assertz(ar_router:route(Method, Route, Module, Goal, Before))).

%% ar_route(+Request) is semidet.
%
% Routes the request into handler or
% calls the fallback handler when no
% matching route is found.
%
% Request must contain method(Method)
% and path(Path).
    
ar_route(Request):-
    memberchk(method(Method), Request),
    memberchk(path(Path), Request),
    path_to_route(Path, Route),
    debug(ar_route, 'dispatch: ~w ~w', [Method, Route]),
    dispatch(Method, Route).

%% dispatch(+Method, +Route) is semidet.
%
% Attempts to dispatch the request.
% Fails when no matching handler is found.
%
% Throws handler_failed(Method, Path) when
% handler was found but it failed during
% execution.
%
% Throws middleware_failed(Goal) when
% a middleware in before-list fails.

dispatch(Method, Path):-
    route(Method, Path, Module, Goal, Before), !,
    (   run_handler(Before, Module, Goal)
    ->  true
    ;   throw(error(handler_failed(Method, Path)))).
    
run_handler([Step|Rest], Module, Goal):- !,
    debug(ar_router, 'calling: ~w', [Step]),
    Next = ar_router:run_handler(Rest, Module, Goal),
    (   call(Module:Step, Next)
    ->  true
    ;   throw(error(middleware_failed(Step)))).
    
run_handler([], Module, Goal):-
    debug(ar_router, 'calling handler: ~w', [Goal]),
    call(Module:Goal), !,
    debug(ar_router, 'handler finished', []).

%% path_to_route(+Path, -Route) is det.
%
% Turns path atom like '/path/to/something' into
% Prolog term path/to/something.
    
path_to_route(Path, Route):-    
    atom_codes(Path, Codes),
    phrase(path_tokens([/|Tokens]), Codes),
    path_to_route_term(Tokens, Route), !.
    
path_to_route_term([], /).
    
path_to_route_term([First|Rest], Term):-
    path_to_route_term(Rest, First, Term).
    
path_to_route_term([/,A|Rest], Acc, Term):-
    path_to_route_term(Rest, /(Acc, A), Term).
    
path_to_route_term([A], Acc, /(Acc, A)).

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

path_char(Char) --> [Char], { Char \= 47 }.
