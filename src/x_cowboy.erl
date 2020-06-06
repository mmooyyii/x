-module(x_cowboy).
-author("MoYi").

%% API
-export([start/2, stop/1, set_rule/2, init/3]).

start(App, Config) ->
    cowboy:start_clear(App, Config, #{}).

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% [{host,[{prefix,module,option}]}]
set_rule(App, PrefixModulePair) ->
    Route = [{Prefix, Module, []} || {Prefix, Module} <- PrefixModulePair],
    Dispatch = cowboy_router:compile([{'_', Route}]),
    ranch:set_protocol_options(App, #{env => #{dispatch => Dispatch}}).

init(Module, Req, Opts) ->
    App = x_global:get_app(port(Req)),
    case x_server:find_route(App, Module, method(Req), path(Req)) of
        {F, Url} ->
            A = x_server:make_args(Url, Req, Opts),
            try
                {ok, x_utils:make_response(apply(Module, F, A), Req), Opts}
            catch
                _:_ ->
                    '500'(Req, Opts)
            end;
        error ->
            io:format("1231231231"),
            '404'(Req, Opts)
    end.


port(#{port:=Port}) ->
    Port.

path(#{path_info:=[Prefix], path := <<Prefix, Return/binary>>}) ->
    Return;
path(#{path := Path}) ->
    Path.
method(#{method :=Method}) ->
    binary_to_atom(http_util:to_lower(Method), utf8).

'404'(Req, Opts) ->
    {ok, cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"">>, Req), Opts}.

'500'(Req, Opts) ->
    {ok, cowboy_req:reply(500, #{<<"content-type">> => <<"text/plain">>}, <<"">>, Req), Opts}.
