-module(x_blueprint).
-author("MoYi").
-include("x.hrl").


-callback blueprint() -> term().
-callback init(term(), term()) -> term().