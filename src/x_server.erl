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

-export([register/1, find_route/4, make_args/3, p_find_route/3]).
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
    case x_cowboy:start(Server, [{port, Port}]) of
        {ok, Pid} ->
            process_flag(trap_exit, true),
            erlang:monitor(process, Pid);
        _ -> ignore
    end,
    {ok, #state{name = Name, config = Config, semaphore = 1000, route = #{}, server = Server}}.

handle_call({find_route, Module, Method, Path}, _From, State = #state{route = Route}) ->
    {Prefix, R} = maps:get(Module, Route),
    Head = x_utils:binary_rstrip(Prefix, <<"/[...]">>),
    io:format("~p~n", [{Method, x_utils:binary_lstrip(Path, Head), R}]),
    Return = p_find_route(Method, x_utils:binary_lstrip(Path, Head), R),
    {reply, Return, State};


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


make_args(_Url, _Req, _Opts) -> [].

register(#meta_route{module = Module, route = Routes, prefix = Prefix}) ->
    F = fun({Fun, {Method, {App, Path}}}, Map) ->
        Ls = maps:get(App, Map, []),
        Map#{App=>[{Method, Path, Fun} | Ls]}
        end,
    register_to_app(Module, Prefix, lists:foldl(F, #{}, Routes)).

register_to_app(Module, Prefix, Apps) ->
    maps:fold(fun(App, Route, _) ->
        update_route(Module, list_to_binary(Prefix), Route, App) end, '_', Apps).

find_route(App, Module, Method, Path) ->
    gen_server:call(App, {find_route, Module, Method, Path}).

update_route(Module, Prefix, Route, App) ->
    gen_server:call(App, {update_route, #{Module => {Prefix, Route}}}).

http_server_name(App) ->
    list_to_atom(atom_to_list(App) ++ "_http_server").

p_find_route(_Method, _Path, []) ->
    error;
p_find_route(Method, Path, [{Method, Match, Func} | Rest]) ->
    %%    TODO: use crit bit tree
    case is_match(Path, Match) of
        true ->
            {Func, Match};
        false ->
            p_find_route(Method,Path, Rest)
    end;
p_find_route(Method, Path, [_ | Rest]) ->
    p_find_route(Method, Path, Rest).

is_match(Path, Match) ->
    M = x_utils:list_strip(binary:split(Match, <<"/">>, [global]), <<"">>),
    P = x_utils:list_strip(binary:split(Path, <<"/">>, [global]), <<"">>),
    case length(M) =:= length(P) of
        true -> p_is_match(M, P);
        false -> false
    end.

p_is_match([], []) ->
    true;
p_is_match([A | M], [A | P]) ->
    p_is_match(M, P);
p_is_match([M1 | M], [_ | P]) ->
    case is_wildcard(M1) of
        true -> p_is_match(M, P);
        false -> false
    end.
is_wildcard(_Word) -> false.
