-module(x_server).
-author("mmooyyii").
-behaviour(gen_server).

-include("x.hrl").
-export([start_link/2]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([register/1]).
-define(SERVER, ?MODULE).

-record(state, {
    name :: atom(),
    config :: list(),
    semaphore :: integer(),
    route :: #{atom() => {binary(), list()}},
    server :: pid()
}).

start_link(Name, Config) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Config], []).

init([Name, Config]) ->
    Port = proplists:get_value(port, Config),
    Server = http_server_name(Name),
    {ok, Pid} = x_cowboy:start(Server, [{port, Port}]),
    {ok, #state{name = Name, config = Config, semaphore = 1000, route = #{}, server = Server}}.

handle_call({update_route, Route1}, _From, State = #state{route = Route2, server = Server}) ->
    NewRoute = maps:merge(Route2, Route1),
    PrefixModulePair = maps:fold(fun(Module, {Prefix, _}, Acc) -> [{Prefix, Module} | Acc] end, [], NewRoute),
    x_cowboy:set_rule(Server, PrefixModulePair),
    {reply, ok, State#state{route = NewRoute}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_route(App, Path) -> ok.
make_args(_, _) -> ok.
find_error_handle(_) -> ok.

%% R1: #{app=> [{prefix,module}]}
%% R2: #{{app,module} => path}.
register(#meta_route{module = Module, route = Routes, prefix = Prefix}) ->
    F = fun({Fun, {Method, {App, Path}}}, Map) ->
        Ls = maps:get(App, Map, []),
        Map#{App=>[{Method, Path, Fun} | Ls]}
        end,
    register_to_app(Module, Prefix, lists:foldl(F, #{}, Routes)).

register_to_app(Module, Prefix, Apps) ->
    maps:fold(fun(App, Route, _) -> update_route(Module, Prefix, Route, App) end, '_', Apps).

update_route(Module, Prefix, Route, App) ->
    gen_server:call(App, {update_route, #{Module => {Prefix, Route}}}).

http_server_name(App) ->
    list_to_atom(atom_to_list(App) ++ "_http_server").

