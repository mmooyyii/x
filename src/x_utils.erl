-module(x_utils).
-author("mmooyyii").

%% API
-export([make_response/1]).


make_response(Req0) ->
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>}, <<"Hello 123!">>,
        Req0).