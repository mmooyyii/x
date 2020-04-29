-module(x_blueprint).
-author("mmooyyii").

-export([]).
-type prefix() :: binary().

%%% blueprint transform in compile time,
%%% route define will append to this function
-callback blueprint() -> prefix().
