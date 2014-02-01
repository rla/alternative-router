:- use_module(library(http/thread_httpd)).
:- use_module(prolog/ar_router).

:- route_get(hello/Name, handle_hello(Name)).

handle_hello(Name):-
    format('Content-Type: text/plain; charset=UTF-8~n~n'),
    format('Hello ~w', [Name]).

:- http_server(ar_route, [port(8008)]).
