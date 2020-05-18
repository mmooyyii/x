-module(x_route).
-author("mmooyyii").

%% crit bit tree
%% https://www.imperialviolet.org/binary/critbit.pdf

-export([new/0, insert/3, delete/2, lookup/2]).

-define(NULL, null).
-type crit_bit_node() :: crit_bit_node().
-type crit_bit_leaf() :: crit_bit_leaf().
-type crit_bit_tree() :: crit_bit_tree().

-record(crit_bit_tree, {root :: crit_bit_node() | crit_bit_leaf()}).
-record(crit_bit_node, {
    children :: {crit_bit_node()|crit_bit_leaf(), crit_bit_node()|crit_bit_leaf()},
    index :: integer(),
    bit :: byte(),
    cont :: boolean() %% Key1 is Key2's prefix,such as key1 "aaaa" in child1, key2 "aaaab" in child2;
}).
-record(crit_bit_leaf, {key :: binary(), value :: term()}).

-spec new() -> crit_bit_tree().
new() -> #crit_bit_tree{root = ?NULL}.

-spec insert(crit_bit_tree(), binary(), term()) -> crit_bit_tree().
insert(#crit_bit_tree{root = ?NULL}, Key, Value) ->
    #crit_bit_tree{root = new_leaf(Key, Value)};
insert(#crit_bit_tree{}, _Key, _Value) ->
    #crit_bit_tree{}.

-spec lookup(crit_bit_tree(), binary()) -> {ok, term()} | {error, not_find}.
lookup(#crit_bit_tree{}, _Key) ->
    {ok, ok}.

-spec delete(crit_bit_tree(), binary()) -> crit_bit_tree().
delete(#crit_bit_tree{}, _Key) ->
    ok.

new_node(Index, Bit) ->
    #crit_bit_node{index = Index, bit = Bit, children = {?NULL, ?NULL}}.

add_child(CN = #crit_bit_node{children = {_, C2}}, Node, 0) ->
    CN#crit_bit_node{children = {Node, C2}};
add_child(CN = #crit_bit_node{children = {C1, _}}, Node, 1) ->
    CN#crit_bit_node{children = {C1, Node}}.

new_leaf(Key, Value) ->
    #crit_bit_leaf{key = Key, value = Value}.

direction(#crit_bit_node{index = Index, bit = Bit, cont = Cont}, Key) ->
    case Index < length(Key) andalso (get_byte(Index, Key) band Bit orelse Cont) of
        true -> 1;
        false -> 0
    end.

critical_bit(#crit_bit_leaf{key = Key1}, Key2) when Key1 > Key2 ->
    L1 = length(Key1),
    L2 = length(Key2),
    case same_head(0, Key1, Key2, min(L1, L2)) of
        {cont, Index} when L1 > L2 ->
            {matrix(get_byte(Index, Key1)), true};
        {cont, Index} when L1 < L2 ->
            {matrix(get_byte(Index, Key2)), true};
        {Bit, Index} ->
            {Index, Bit, false}
    end.

same_head(Index, _, _, Max) when Index > Max ->
    {cont, Index - 1};
same_head(Index, Key1 = <<Head:Index/binary, _/bits>>, Key2 = <<Head:Index/binary, _/bits>>, Max) ->
    same_head(Index + 1, Key1, Key2, Max);
same_head(Index, Key1 = <<_:Index/binary, _/bits>>, Key2 = <<_:Index/binary, _/bits>>, _) ->
    {matrix(get_byte(Index, Key1) bxor get_byte(Index, Key2)), Index - 1}.

get_byte(Index, <<_:Index/binary, Rt:8, _/bits>>) -> Rt.

matrix(<<1:1, _:1, _:1, _:1, _:1, _:1, _:1, _:1>>) -> 128;
matrix(<<0:1, 1:1, _:1, _:1, _:1, _:1, _:1, _:1>>) -> 64;
matrix(<<0:1, 0:1, 1:1, _:1, _:1, _:1, _:1, _:1>>) -> 32;
matrix(<<0:1, 0:1, 0:1, 1:1, _:1, _:1, _:1, _:1>>) -> 16;
matrix(<<0:1, 0:1, 0:1, 0:1, 1:1, _:1, _:1, _:1>>) -> 8;
matrix(<<0:1, 0:1, 0:1, 0:1, 0:1, 1:1, _:1, _:1>>) -> 4;
matrix(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1, _:1>>) -> 2;
matrix(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 1:1>>) -> 1;
matrix(<<0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1>>) -> 0.
