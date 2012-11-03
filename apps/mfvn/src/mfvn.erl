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
    incr/2,
    get_debug_preflist/2,
    get_debug_preflist/3
  ]).

-define(TIMEOUT, 5000).

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
  {ok, ReqId} = mfvn_get_fsm:get(Client, StatName),
  wait_for_reqid(ReqId, ?TIMEOUT).

get_debug_preflist(Client, StatName) ->
  DocIdx = riak_core_util:chash_key({list_to_binary(Client), list_to_binary(StatName)}),
  riak_core_apl:get_apl(DocIdx, ?N, mfvn_stat).

get_debug_preflist(Client, StatName, N) ->
  IdxNode = lists:nth(N, get_debug_preflist(Client, StatName)),
  {ok, req_id, Val} = riak_core_vnode_master:sync_command(IdxNode, {
      get, req_id, StatName}, mfvn_stat_vnode_master),
  [IdxNode, Val].

set(Client, StatName, Val) ->
  do_write(Client, StatName, set, Val).

sadd(Client, StatName, Val) ->
  do_write(Client, StatName, sadd, Val).

incr(Client, StatName) ->
  do_write(Client, StatName, incr).

do_write(Client, StatName, Op) ->
  {ok, ReqId} = mfvn_write_fsm:write(Client, StatName, Op),
  wait_for_reqid(ReqId, ?TIMEOUT).

do_write(Client, StatName, Op, Val) ->
  {ok, ReqId} = mfvn_write_fsm:write(Client, StatName, Op, Val),
  wait_for_reqid(ReqId, ?TIMEOUT).

wait_for_reqid(ReqId, Timeout) ->
  receive
    {ReqId, ok} -> ok;
    {ReqId, ok, Val} -> {ok, Val}
  after Timeout -> {error, timeout}
  end.
