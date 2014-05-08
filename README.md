# alternative-router

This is an alternative router/dispatcher to Swi-Prolog's
[http_dispatch](http://www.swi-prolog.org/pldoc/man?section=httpdispatch) module. The
main motivation for creating this module was more convenient (less verbose) implementation
of RESTful web services.

## Example

    :- use_module(library(http/thread_httpd)).
    :- use_module(library(arouter)).

    :- route_get(hello/Name, handle_hello(Name)).

    handle_hello(Name):-
        format('Content-Type: text/plain; charset=UTF-8~n~n'),
        format('Hello ~w', [Name]).

    :- http_server(route, [port(8008)]).

Save it to a file, run it, and then visit <http://localhost:8008/hello/world>.

## Path terms

Normal path terms correspond directly to URL paths with the implicit root (`/`)
symbol. For example, `path/to/something` corresponds to URL
`http://example.com/path/to/something`. To match the root itself, `/` alone
must be used. To match an URL path with a slash in the end, an empty atom
has to be used at the end of the path term. For example, to match URL
`http://example.com/path/to/something/`, a path term `path/to/something/''`
must be used.

Why not "prettier" path terms? It is possible by overloading `/` as prefix and postfix
operator. Then terms like `/something` and `/something/to/` can be used. However, defining
single-symbol operators that are likely to clash with other modules has been intentionally
avoided. In practical usage, most paths are likely to be normal paths anyway. If you do
not agree with this then open an issue on the tracker.

## Using with http_dispatch

Make fallback to `http_dispatch/1` like this:

    handle_request(Request):-
    (   route(Request)
    ->  true
    ;   http_dispatch(Request)).
    
and use `handle_request/1` as the handler in `http_server`.

## Before-handler

Routes can have intermediate goals. The following example is cheking auth information
before executing the handler:

    :- route_get(api/resource, auth, handle_resource).

    auth(Next):-
    (   http_session_data(user(User)),
        memberchk(role(admin), User)
    ->  call(Next)
    ;   current_request(Request),
        memberchk(path(Path), Request),
        current_output(Out),
        http_reply(forbidden(Path), Out, [])).
    
    handle_resource:-
        ...

The before-handler predicate calls its first argument when the request should pass it.
Otherwise it should produce the response itself.

## Route-path matching

A path will match the route when:

 * Path and rule are `/`.
 * Path is an atomic value and the route is same atomic value or a variable.
 * Path is `/(P1, P2)`, route is `/(R1, R2)` and R1 matches P1 and R2 matches P2.

## Route priority

Routes are tested in reverse to the adding order. Overlapping routes can be added. Structurally equivalent
routes cannot be added. Routes are overlapping when one route has atom in position of route variable in the
other. Routes are structurally equivalent when:

 * Both routes are `/`.
 * Both routes are variables.
 * Both routes are same atomic values.
 * One route is `/(A1, B1)`, the other is `/(A2, B2)` and
   A1, A2 are structurally equivalent and B1, B2 are structurally equivalent.

Structural equivalence is used for detecting duplicate rules. This plays nice with the `make/0` goal.

## List of predicates

### Adding new routes

`route_get(+Route, :Goal)` registers a new GET handler.

`route_put(+Route, :Goal)` registers a new PUT handler.

`route_del(+Route, :Goal)` registers a new DELETE handler.

`route_post(+Route, :Goal)` registers a new POST handler.

`route_get(+Route, :Before, :Goal)` registers a new GET handler with a before action.

`route_put(+Route, :Before, :Goal)` registers a new PUT handler with a before action.

`route_del(+Route, :Before, :Goal)` registers a new DELETE handler with a before action.

`route_post(+Route, :Before, :Goal)` registers a new POST handler with a before action.

`new_route(+Method, +Route, :Goal)` registers a new custom method handler.

`new_route(+Method, +Route, :Before, :Goal)` registers a new custom method handler with a before action.

All predicates above will throw an error when the `Route` does not contain the
suitable term.

Route handler predicates can take variables from the route. Example:

    :- http_get(post/show/Slug, post_show(Slug)).
    
    post_show(Slug):-
        ...

However, they do not take the `Request` argument, unlike the `http_dispatch` handlers. To obtain the
current request, use the [http_current_request/1](http://www.swi-prolog.org/pldoc/doc_for?object=http_current_request/1)
predicate.

### Dispatching

`route(+Request)` - takes given request and attempts to find suitable handler.

Request must contain terms `method(Method)` and `path(Path)`. Throws `handler_failed(Method, Path)` when
handler was found but it failed during execution.

### Inspecting routes

Use the `route(?Method, ?Route, ?Before, ?Goal)` predicate.

### Removing routes

Use the `route_remove(Method, Route)` predicate. Both arguments
can be unbound or partially instantiated.

## Installation

To install as a package:

    pack_install(arouter).

Tested with Swi-Prolog 7.x but should work with earlier versions too.

## Full API documentation

See <http://packs.rlaanemets.com/alternative-router/doc/>.

## Running tests

In the package root, insert into swipl:

    [tests/tests].
    run_tests.

Or if you cloned the repo:

    make test

## Debugging

Enable debugging with:

    ?- use_module(library(debug)).
    ?- debug(arouter).

## Changelog

 * 2014-05-08 version 1.0.0. Precise route matching semantics.
 * 2014-02-01 version 0.0.1

## Bug reports/feature requests

Please send bug reports/feature request through the GitHub
project [page](https://github.com/rla/alternative-router).

## License

The MIT license. See the LICENSE file.
