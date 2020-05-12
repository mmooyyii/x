-module(x).
-author("yimo").

%% API
-export([start_app/2, cowboy_init/2]).

cowboy_init(Req0, Opts) ->
    x_cowboy:init(Req0, Opts).

start_app(App, Config) ->
    Port = proplists:get_value(port, Config),
    {ok, Pid} = x_server:start_link(App, Config),
    x_global:set_app(Port, Pid),
    {ok, Pid}.


