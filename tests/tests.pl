:- begin_tests(ar_router).
:- use_module(prolog/ar_router).

:- dynamic(visited/1).

:- route_get(/, index).

index:-
    assertz(visited(index)).
    
:- route_get(a, handle_a).

handle_a:-
    assertz(visited(a)).
    
:- route_get(hello/X, handle_hello(X)).

handle_hello(X):-
    assertz(visited(hello(X))).
    
fallback_fail(_):-
    fail.

test(path1):-
    path_to_route('/', /).
    
test(path2):-
    path_to_route('/a', a).
    
test(path3):-
    path_to_route('/a/b', a/b).
    
test(path4):-
    path_to_route('/a/b/c', a/b/c).
    
test(path5):-
    path_to_route('/a/b/c/', a/b/c/(/)).
    
test(index, [ setup(retractall(visited(_))) ]):-
    ar_route([ path('/'), method(get) ]),
    visited(index).
    
test(a, [ setup(retractall(visited(_))) ]):-
    ar_route([ path('/a'), method(get) ]),
    visited(a).
    
test(hello, [ setup(retractall(visited(_))) ]):-
    ar_route([ path('/hello/world'), method(get) ]),
    visited(hello(world)).

:- end_tests(ar_router).
