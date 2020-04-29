-module(x_blueprint).
-author("MoYi").


-include("x.hrl").
-type prefix() :: binary().

-callback blueprint() -> prefix().
-callback init() -> no_return().

