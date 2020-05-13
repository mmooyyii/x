-module(x_cowboy).
-author("MoYi").

%% API
-export([start/2, stop/1, set_rule/2, init/2]).

start(App, Config) ->
    cowboy:start_clear(App, Config, #{}).

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% [{host,[{prefix,module,option}]}]
set_rule(App, PrefixModulePair) ->
    Route = [{Prefix, Module, []} || {Prefix, Module} <- PrefixModulePair],
    Dispatch = cowboy_router:compile([{'_', Route}]),
    ranch:set_protocol_options(App, #{env => #{dispatch => Dispatch}}).

init(Req, Opts) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello world!">>, Req),
    {ok, Req, Opts}.
%%    App = x_global:get_app(port(Req)),
%%    {M, F} = x_server:find_route(App, path(Req)),
%%    A = x_server:make_args(Req, Opts),
%%    Rt = try
%%             apply(M, F, A)
%%         catch
%%             Error:Reason:Stack ->
%%                 {ErrorModule, ErrorFunc} = x_server:find_error_handle(App),
%%                 apply(ErrorModule, ErrorFunc, [{Error, Reason, Stack}])
%%         end,
%%    {ok, x_utils:make_response(Rt), Opts}.

port(Req) ->
    58888.

path(Req) -> <<"/">>.
