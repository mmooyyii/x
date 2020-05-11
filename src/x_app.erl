-module(x_app).
-behaviour(application).

%% API.
-export([start/0]).
-export([start/2]).
-export([stop/1]).

%% API.
start() ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(cowlib),
    ok = application:start(ssl),
    ok = application:start(jesse),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(x).

start(_Type, _Args) ->
    ets:new(abc, [set, named_table]),
    x_sup:start_link().

stop(_State) ->
    exit(x_sup, shutdown).

