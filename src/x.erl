-module(x).
-author("yimo").

%% API
-export([start_app/2, cowboy_init/3]).

cowboy_init(Module, Req0, Opts) ->
    x_cowboy:init(Module, Req0, Opts).

start_app(App, Config) ->
    Port = proplists:get_value(port, Config),
    Pid = case x_server:start_link(App, Config) of
              {ok, P} -> P;
              {error, {already_started, P}} -> P
          end,
    x_utils:load_blueprint(App),
    x_global:set_app(Port, App),
    {ok, Pid}.
