-module(x_server).
-author("mmooyyii").
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([regist/1]).
-define(SERVER, ?MODULE).

-record(state, {
    name :: atom(),
    config :: list(),
    semaphore :: integer()
}).

start_link(Name, Config) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Config], []).

init([Name, Config]) ->
    {ok, #state{name = Name, config = Config}}.

handle_call(_Request, _From, State) ->
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

set_config(Name, Config) ->
    ok.

run() ->
    ok.

stop() ->
    ok.

find_app(Port) ->
    ok.

find_route(App) -> ok.
make_args(_, _) -> ok.
find_error_handle(_) -> ok.

regist(V) ->
    io:format("this is regist:~p~n", [V]).
