-module(x_transform).
-author("MoYi").


-export([parse_transform/2]).

parse_transform(AST, _Option) ->
    case is_x_bp(AST) of
        true ->
            put(route, []),
            p_parse_transform(AST, []);
        _ ->
            AST
    end.

p_parse_transform([], NewAST) ->
    lists:sort(fun(A, B) -> element(2, A) < element(2, B) end, [add_bp() | NewAST]);
p_parse_transform([Part | AST], NewAST) ->
    case match_http_method(Part) orelse match_blueprint(Part) of
        true ->
            p_parse_transform(AST, NewAST);
        false ->
            match_api(Part),
            p_parse_transform(AST, [Part | NewAST])
    end.

match_http_method({attribute, _Line, Method, Args})
    when Method =:= get orelse Method =:= post
    orelse Method =:= put orelse Method =:= delete ->
    put(method, {Method, Args}),
    true;
match_http_method(_) ->
    false.

match_api({function, _Line, Function, _, _Body}) ->
    case get(method) of
        undefined ->
            pass;
        {Method, Args} ->
            Ls = get(route),
            put(route, [{Function, {Method, Args}} | Ls]),
            erase(method)
    end;
match_api(_) ->
    pass.

match_blueprint({function, _Line, blueprint, 0, _Body} = Bp) ->
    undefined = get(store),
    put(bp, Bp),
    true;
match_blueprint(_) -> false.

add_bp() ->
    get(bp).

is_x_bp(AST) -> lists:any(fun match_behavior/1, AST).
match_behavior({attribute, _, behavior, x_blueprint}) -> true;
match_behavior({attribute, _, behaviour, x_blueprint}) -> true;
match_behavior(_) -> false.

