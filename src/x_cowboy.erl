-module(x_cowboy).
-author("MoYi").

%% API
-export([start/0, stop/1, set_rule/1, init/2]).

start() ->
    cowboy:start_clear(http, [{port, 58887}], #{}).

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% [{host,[{prefix,module,option}]}]
%% [{'_', [{"/123", aaaa, []}]}]
set_rule(Rule) ->
    Dispatch = cowboy_router:compile([{'_', [{"/:name", aaaa, []}]}]),
    ranch:set_protocol_options(http, #{env => #{dispatch => Dispatch}}).


init(Req0, Opts) ->
    App = x_server:find_app(1),
    {M, F} = x_server:find_route(App),
    A = x_server:make_args(Req0, Opts),
    Rt = try
             apply(M, F, A)
         catch
             Error:Reason:Stack ->
                 {ErrorModule, ErrorFunc} = x_server:find_error_handle(App),
                 apply(ErrorModule, ErrorFunc, [{Error, Reason, Stack}])
         end,
%%    Req = cowboy_req:reply(200,
%%    #{<<"content-type">> => <<"text/plain">>},
%%    <<"Hello 123!">>, Req0),
    {ok, x_utils:make_response(Rt), Opts}.
