-module(x_blueprint).
-author("MoYi").

-export([]).
-type prefix() :: binary().

%% blueprint transform in compile time.
-callback blueprint() -> prefix().
