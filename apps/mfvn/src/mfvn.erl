-module(mfvn).
-include("mfvn.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         entry/2
        ]).

-export([
    get/2,
    set/3,
    sadd/3,
    incr/2
  ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, mfvn),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, mfvn_vnode_master).

entry(Client, Entry) ->
  DocIdx = riak_core_util:chash_key({list_to_binary(Client), term_to_binary(now())}),
  PrefList = riak_core_apl:get_apl(DocIdx, 1, mfvn_entry),
  [IdxNode] = PrefList,
  mfvn_entry_vnode:entry(IdxNode, Client, Entry).

get(Client, StatName) ->
  mfvn_stat_vnode:get(getidxnode(Client, StatName), StatName).

set(Client, StatName, Val) ->
  mfvn_stat_vnode:set(getidxnode(Client, StatName), StatName, Val).

sadd(Client, StatName, Val) ->
  mfvn_stat_vnode:sadd(getidxnode(Client, StatName), StatName, Val).

incr(Client, StatName) ->
  mfvn_stat_vnode:incr(getidxnode(Client, StatName), StatName).

getidxnode(Client, StatName) ->
  DocIdx = riak_core_util:chash_key({list_to_binary(Client), list_to_binary(StatName)}),
  hd(riak_core_apl:get_apl(DocIdx, 1, mfvn_stat)).
