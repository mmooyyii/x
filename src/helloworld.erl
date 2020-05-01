-module(helloworld).

-behavior(x_blueprint).
-author("MoYi").
-compile([{parse_transform, x_transform}]).
%% API
-export([blueprint/0]).
-export([hello/0]).

blueprint() ->
    <<"/">>.

-get({app, <<"/">>}).
hello() ->
    #{<<"hello">> => <<"world">>}.


run() ->
    x:set_web_app(app, [{ip, '127.0.0.1'}, {port, 57777}]),
    x:run(app).