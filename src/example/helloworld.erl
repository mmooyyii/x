-module(helloworld).

-behavior(x_blueprint).
-author("MoYi").
-compile([{parse_transform, x_transform}]).
%% API
-export([blueprint/1, init/2]).
-export([hello/0, login/0, hello2/0]).
-export([start/0]).

blueprint(app) ->
    <<"/123">>.
init(Req, Option) ->
    x:cowboy_init(?MODULE, Req, Option).

-get({app, <<"/">>}).
hello() ->
    <<"hello world">>.

-get({app, <<"/1233">>}).
hello2() ->
    <<"hello world2">>.

-post({app, <<"/login/">>}).
login() ->
    <<"#{<<\"hello\">> => <<\"world\">>}">>.

start() ->
    x:start_app(app, [{host, "127.0.0.1"}, {port, 9999}, {debug, true}]).
