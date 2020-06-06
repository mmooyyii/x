-module(x_blueprint).
-author("MoYi").

-callback blueprint(atom()) -> binary().
-callback init(term(), term()) -> term().