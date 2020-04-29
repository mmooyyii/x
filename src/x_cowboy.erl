-module(x_cowboy).
-author("MoYi").

%% API
-export([start/0, stop/1, reset_rule/0]).

start() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_h, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 58887}], #{
        env => #{dispatch => Dispatch}
    }),
    hello_world_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).

reset_rule() ->
    Dispatch = cowboy_router:compile([{'_', [{"/123", aaaa, []}]}]),
    ranch:set_protocol_options(http, #{env => #{dispatch => Dispatch}}).
