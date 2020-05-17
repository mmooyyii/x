-author("yimo").

-type method() :: get | post | put | delete.
-type app() :: atom().
-type path() :: binary().

-record(meta_route, {
    module :: atom(),
    prefix :: binary(),
    route :: [{atom(), {method(), {app(), path()}}}|_]
}).

-define(PortToApp, x_port_to_app).