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
    route :: #{atom() => {binary(), list()}}
}).

start_link(Name, Config) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Config], []).

init([Name, Config]) ->
    {ok, #state{name = Name, config = Config}}.

handle_call({regist, MetaRoute}, _From, State) when is_record(MetaRoute, meta_route) ->
    #meta_route{module = Module, route = Route, prefix = Prefix} = MetaRoute,

    {reply, ok, State}.

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
    F = fun([{Fun, {Method, {App, Path}}}], Map) ->
        Ls = maps:get(App, Map, []),
        Map#{App=>[{Method, Path, Fun} | Ls]}
        end,
    Apps = lists:foldl(F, #{}, Routes).




