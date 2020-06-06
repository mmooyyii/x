-module(x_app).
-behaviour(application).

%% API.
-export([start/0]).
-export([start/2]).
-export([stop/1]).

%% API.
start() ->
    application:start(asn1),
    application:start(crypto),
    application:start(public_key),
    application:start(cowlib),
    application:start(ssl),
    application:start(jesse),
    application:start(ranch),
    application:start(cowboy),
    application:start(x).


start(_Type, _Args) ->
    x_global:new(),
    helloworld:start(),
    x_sup:start_link().

stop(_State) ->
    exit(x_sup, shutdown).

