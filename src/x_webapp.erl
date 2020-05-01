-module(x_webapp).
-author("yimo").

-behaviour(gen_server).

-export([start_link/0]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(x_webapp_state, {}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #x_webapp_state{}}.

handle_call(_Request, _From, State = #x_webapp_state{}) ->
    {reply, ok, State}.
handle_cast(_Request, State = #x_webapp_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #x_webapp_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #x_webapp_state{}) ->
    ok.
code_change(_OldVsn, State = #x_webapp_state{}, _Extra) ->
    {ok, State}.

