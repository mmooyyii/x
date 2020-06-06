-module(x_utils).
-author("mmooyyii").

%% API
-export([
    make_response/2,
    load_blueprint/1,
    binary_lstrip/2,
    binary_rstrip/2,
    binary_strip/2,
    list_lstrip/2,
    list_rstrip/2,
    list_strip/2
]).


make_response(Return, Req0) ->
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Return, Req0).

load_blueprint(App) ->
    Modules = (erlang:loaded() -- sync_utils:get_system_modules()),
    lists:foreach(fun(Module) ->
        Behaviors = proplists:get_value(behavior, Module:module_info(attributes), []),
        case lists:member(x_blueprint, Behaviors) of
            true ->
                p_load_blueprint(Module, App);
            _ ->
                ignore
        end end, Modules).

p_load_blueprint(Module, App) ->
    try
        Module:blueprint(App)
    catch
        error:function_clause -> ignore;
        _:Error -> io:format(Error)
    end.


binary_lstrip(Binary, Strip) ->
    Head = binary:longest_common_prefix([Binary, Strip]),
    case Head =:= size(Strip) of
        true ->
            A = 8 * Head,
            <<_:A, B/binary>> = Binary,
            B;
        false ->
            Binary
    end.

binary_rstrip(Binary, Strip) ->
    binary_reverse(binary_lstrip(binary_reverse(Binary), binary_reverse(Strip))).
binary_strip(B, S) -> binary_rstrip(binary_lstrip(B, S), S).

binary_reverse(Binary) ->
    list_to_binary(lists:reverse(binary_to_list(Binary))).

list_lstrip([], _) -> [];
list_lstrip([S | R], S) -> R;
list_lstrip([S1 | R], _S2) -> [S1 | R].
list_rstrip(R, S) -> lists:reverse(list_lstrip(lists:reverse(R), S)).
list_strip(R, S) -> list_rstrip(list_lstrip(R, S), S).