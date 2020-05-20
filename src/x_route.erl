-module(x_route).
-author("mmooyyii").

%% crit bit tree
%% https://www.imperialviolet.org/binary/critbit.pdf

-export([new/0, insert/3, delete/2, lookup/2]).

-define(NULL, null).
-type crit_bit_node() :: crit_bit_node().
-type crit_bit_leaf() :: crit_bit_leaf().
-type crit_bit_tree() :: crit_bit_tree().

-record(crit_bit_tree, {
    root :: crit_bit_node() | crit_bit_leaf(),
    size :: integer()
}).
-record(crit_bit_node, {
    children :: {crit_bit_node()|crit_bit_leaf(), crit_bit_node()|crit_bit_leaf()},
    index :: integer(),
    bit :: byte(),
    cont :: boolean() %% Key1 is Key2's prefix,such as key1 "aaaa" in child1, key2 "aaaab" in child2;
}).
-record(crit_bit_leaf, {key :: binary(), value :: term()}).

-spec new() -> crit_bit_tree().
new() -> #crit_bit_tree{root = ?NULL, size = 0}.

-spec insert(crit_bit_tree(), binary(), term()) -> crit_bit_tree().
insert(#crit_bit_tree{root = ?NULL}, Key, Value) ->
    #crit_bit_tree{root = new_leaf(Key, Value), size = 1};
insert(#crit_bit_tree{root = Root, size = Size}, Key, Value) ->
    NewRoot = check_node(Root, Key, Value),
    case get(node) of
        #crit_bit_node{} = Node ->
            erase(),
            #crit_bit_tree{root = add_node(Root, Node, Key), size = Size + 1};
        undefined ->
            #crit_bit_tree{root = NewRoot}
    end.

-spec lookup(crit_bit_tree(), binary()) -> {ok, term()} | {error, not_find}.
lookup(#crit_bit_tree{}, _Key) ->
    {ok, ok}.

-spec delete(crit_bit_tree(), binary()) -> crit_bit_tree().
delete(#crit_bit_tree{}, _Key) ->
    ok.

check_node(#crit_bit_node{children = {C1, C2}} = Node, Key, Value) ->
    case direction(Node, Key) of
        1 -> Node#crit_bit_node{children = {check_node(C1, Key, Value), C2}};
        2 -> Node#crit_bit_node{children = {C1, check_node(C2, Key, Value)}}
    end;

check_node(#crit_bit_leaf{} = Leaf, Key, Value) ->
    case critical_bit(Leaf, Key) of
        {Index, Bit, -1} ->
            Leaf#crit_bit_leaf{value = Value};
        {Index, Bit, Cont} ->
            Node = #crit_bit_node{children = Children} = new_node(Index, Bit, Cont),
            Direction = direction(Node, Key),
            put(node, Node#crit_bit_node{children = setelement(Direction, Children, new_leaf(Key, Value))})
    end.


add_node(#crit_bit_leaf{} = Node, #crit_bit_node{children = Children} = NewNode, Key) ->
    Direction = direction(NewNode, Key),
    NewNode#crit_bit_node{children = setelement(3 - Direction, Children, Node)};
add_node(
    #crit_bit_node{children = Children1, index = Index1, bit = Bit1} = Node,
    #crit_bit_node{index = Index2, bit = Bit2, children = Children2} = NewNode, Key) ->
    Direction = direction(NewNode, Key),
    case Index1 > Index2 orelse (Index1 =:= Index2 andalso Bit1 < Bit2) of
        true ->
            NewNode#crit_bit_node{children = setelement(3 - Direction, Children2, Node)};
        false ->
            New = add_node(element(Direction, Children1), NewNode, Key),
            Node#crit_bit_node{children = setelement(Direction, Children1, New)}
    end.

new_node(Index, Bit, Cont) ->
    #crit_bit_node{cont = Cont, index = Index, bit = Bit, children = {?NULL, ?NULL}}.

new_leaf(Key, Value) ->
    #crit_bit_leaf{key = Key, value = Value}.

direction(#crit_bit_node{index = Index, bit = Bit, cont = Cont}, Key) ->
    case Index < length(Key) andalso (get_byte(Index, Key) band Bit orelse Cont) of
        true -> 2;
        false -> 1
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
