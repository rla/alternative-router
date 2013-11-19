# alternative-router

This is an alternative router/dispatcher to Swi-Prolog's
[http_dispatch](http://www.swi-prolog.org/pldoc/man?section=httpdispatch) module. The
main motivation for creating this module was more convenient (less verbose) implementation
of RESTful web services.

## Example

    :- use_module(library(http/thread_httpd)).
    :- use_module(library(ar_router)).

    :- route_get(hello/Name, handle_hello(Name)).

    handle_hello(Name):-
        format('Content-Type: text/plain; charset=UTF-8~n~n'),
        format('Hello ~w', [Name]).

    :- http_server(ar_route, [port(8008)]).

## Using with http_dispatch

Make fallback to `http_dispatch/1` like this:

    handle_request(Request):-
    (   ar_route(Request)
    ->  true
    ;   http_dispatch(Request)).
    
and use `handle_request/1` as the handler in `http_server`.

## Middlewares

Routes can have intermediate goals. Example is cheking auth information
before executing the handler:

    :- route_get(api/resource, [auth], handle_resource).

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

The middleware predicate calls its first argument when the request should pass the middleware.
Otherwise it should produce response itself.

## List of predicates

Adding new routes:

 * `route_get(+Route, :Goal)` - registers a new GET handler.
 * `route_put(+Route, :Goal)` - registers a new PUT handler.
 * `route_del(+Route, :Goal)` - registers a new DELETE handler.
 * `route_post(+Route, :Goal)` - registers a new POST handler.
 * `route_get(+Route, +Before:list, :Goal)` - registers a new GET handler. Takes the list of middleware predicates to run.
 * `route_put(+Route, +Before:list, :Goal)` - registers a new PUT handler. Takes the list of middleware predicates to run.
 * `route_del(+Route, +Before:list, :Goal)` - registers a new DELETE handler. Takes the list of middleware predicates to run.
 * `route_post(+Route, +Before:list, :Goal)` - registers a new POST handler. Takes the list of middleware predicates to run.

Route handler predicates can take extra argument or variables from the route. Example:

    :- http_get(post/show/Slug, post_show(Slug)).
    
    post_show(Slug):-
        ...

However, they do not take the `Request` argument, unlike the `http_dispatch` handlers. To obtain the
current request, use the [http_current_request/1](http://www.swi-prolog.org/pldoc/doc_for?object=http_current_request/1)
predicate.

Dispatching:

`ar_route(+Request)` - takes given request and attempts to find suitable handler.

Request must contain terms `method(Method)` and `path(Path)`. Throws `handler_failed(Method, Path)` when
handler was found but it failed during execution. Throws `middleware_failed(Goal)` when
a middleware in before-list fails.

## License

The MIT license. See the LICENSE file.