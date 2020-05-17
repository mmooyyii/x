-module(x_blueprint).
-author("MoYi").
-include("x.hrl").


-callback blueprint() -> binary().
-callback init(term(), term()) -> term().