-module(x_app).
-behaviour(application).

%% API.
-export([start/0]).
-export([start/2]).
-export([stop/1]).

%% API.
start() ->
%%    TODO: 以后改
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
    {ok, self()}.

stop(_State) ->
    ok = cowboy:stop_listener(http).
