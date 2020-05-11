-module(x_sup).
-author("mmooyyii").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1, start_app/2]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_) ->
    {ok, {{simple_one_for_one, 0, 1},
        [{webapp, {x_server, start_link, []},
            temporary, brutal_kill, worker, [call]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_app(Name, Config) ->
    supervisor:start_child(?MODULE, [Name, Config]).