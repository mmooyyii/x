-module(x_global).
-author("mmooyyii").

%% API
-export([new/0, set_app/2, get_app/1, delete_app/1]).

-define(XGlobal, x_global_ets).

new() ->
    ets:new(?XGlobal, [set, named_table]).

set_app(Port, App) ->
    ets:insert(?XGlobal, {Port, App}).
get_app(Port) ->
    case ets:lookup(?XGlobal, Port) of
        [{Port, App}] ->
            App;
        _ ->
            null
    end.
delete_app(Port) ->
    ets:delete(?XGlobal, Port).
