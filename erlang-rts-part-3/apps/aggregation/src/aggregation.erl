-module(aggregation).
-include("aggregation.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
	 accounting/0
        ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, aggregation),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, aggregation_vnode_master).

accounting() ->
    DocIdx = riak_core_util:chash_key({<<"accounting">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, aggregation),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, accounting, aggregation_vnode_master).


