:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(prolog/ar_router).

:- route_get(hello/Name, handle_hello(Name)).

handle_hello(Name):-
    format('Content-Type: text/plain; charset=UTF-8~n~n'),
    format('Hello ~w', [Name]).

:- http_server(handle_request, [port(8008)]).

handle_request(Request):-
    (   ar_route(Request)
    ->  true
    ;   http_dispatch(Request)).
