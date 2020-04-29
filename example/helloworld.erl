-module(helloworld).

-behavior(x_blueprint).
-author("mmooyyii").
-compile([{parse_transform, x_transform}]).

%% API
-export([blueprint/0]).
-export([hello/0]).

blueprint() ->
    <<"/">>.

-get(<<"/index">>).
hello() ->
    #{<<"hello">> => <<"world">>}.



