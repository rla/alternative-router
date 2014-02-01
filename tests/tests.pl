:- begin_tests(arouter).
:- use_module(prolog/arouter).

:- dynamic(visited/1).
:- dynamic(before/1).

% Root handler.

:- route_get(/, index).

index:-
    assertz(visited(/)).
    
% Handlers for normal paths.

:- route_get(a, get_a).

get_a:-
    assertz(visited(get(a))).

:- route_post(a, post_a).

post_a:-
    assertz(visited(post(a))).

:- route_put(a, put_a).

put_a:-
    assertz(visited(put(a))).

:- route_del(a, del_a).

del_a:-
    assertz(visited(del(a))).

% Handlers with a before action.

:- route_get(b, before(get_b), before_get_b).

before_get_b:-
    assertz(visited(get(b))).

:- route_post(b, before(post_b), before_post_b).

before_post_b:-
    assertz(visited(post(b))).

:- route_put(b, before(put_b), before_put_b).

before_put_b:-
    assertz(visited(put(b))).

:- route_del(b, before(del_b), before_del_b).

before_del_b:-
    assertz(visited(del(b))).

before(Token, Goal):-
    assertz(before(Token)),
    call(Goal).

% Handler for dynamic path.

:- route_get(hello/X, handle_hello(X)).

handle_hello(X):-
    assertz(visited(hello(X))).

% Custom method.

:- new_route(options, test/custom, test_custom).

test_custom:-
    assertz(visited(custom)).

clean:-
    retractall(visited(_)),
    retractall(before(_)).

test(path1):-
    path_to_route('/', /).
    
test(path2):-
    path_to_route('/a', a).
    
test(path3):-
    path_to_route('/a/b', a/b).
    
test(path4):-
    path_to_route('/a/b/c', a/b/c).
    
test(path5):-
    path_to_route('/a/b/c/', a/b/c/'').
    
test(index, [ setup(clean) ]):-
    route([ path('/'), method(get) ]),
    visited(/).

test(get_a, [ setup(clean) ]):-
    route([ path('/a'), method(get) ]),
    visited(get(a)).

test(post_a, [ setup(clean) ]):-
    route([ path('/a'), method(post) ]),
    visited(post(a)).

test(put_a, [ setup(clean) ]):-
    route([ path('/a'), method(put) ]),
    visited(put(a)).

test(del_a, [ setup(clean) ]):-
    route([ path('/a'), method(delete) ]),
    visited(del(a)).

test(before_get_b, [ setup(clean) ]):-
    route([ path('/b'), method(get) ]),
    visited(get(b)),
    before(get_b).

test(before_post_b, [ setup(clean) ]):-
    route([ path('/b'), method(post) ]),
    visited(post(b)),
    before(post_b).

test(before_put_b, [ setup(clean) ]):-
    route([ path('/b'), method(put) ]),
    visited(put(b)),
    before(put_b).

test(before_del_b, [ setup(clean) ]):-
    route([ path('/b'), method(delete) ]),
    visited(del(b)),
    before(del_b).

test(hello, [ setup(clean) ]):-
    route([ path('/hello/world'), method(get) ]),
    visited(hello(world)).

test(custom, [ setup(clean) ]):-
    route([ path('/test/custom'), method(options) ]),
    visited(custom).

test(invalid_route):-
    catch((route_get(a(123), _), fail), error(invalid_route(_)), true).

test(remove):-
    route_get(test/remove, true),
    route(get, test/remove, _, _),
    route_remove(_, test/remove),
    (   route(get, test/remove, _, _)
    ->  fail
    ;   true).

:- end_tests(arouter).
