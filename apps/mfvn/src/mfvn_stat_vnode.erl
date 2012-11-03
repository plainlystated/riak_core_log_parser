-module(mfvn_stat_vnode).
-behaviour(riak_core_vnode).
-include_lib("riak_core/include/riak_core_vnode.hrl").
-export([
    encode_handoff_item/2,
    delete/1,
    init/1,
    handle_command/3,
    terminate/2,
    handle_coverage/4,
    handle_exit/3,
    handle_handoff_command/3,
    handle_handoff_data/2,
    handoff_cancelled/1,
    handoff_finished/2,
    handoff_starting/2,
    is_empty/1
  ]).

-export([
    get/2,
    set/3,
    sadd/3,
    incr/2
  ]).

-record(state, {partition, stats}).

-define(MASTER, mfvn_stat_vnode_master).
-define(sync(PrefList, Command, Master),
  riak_core_vnode_master:sync_command(PrefList, Command, Master)).

get(IdxNode, StatName) ->
  ?sync(IdxNode, {get, StatName}, ?MASTER).
set(IdxNode, StatName, Val) ->
  ?sync(IdxNode, {set, StatName, Val}, ?MASTER).
sadd(IdxNode, StatName, Val) ->
  ?sync(IdxNode, {sadd, StatName, Val}, ?MASTER).
incr(IdxNode, StatName) ->
  ?sync(IdxNode, {incr, StatName}, ?MASTER).

init([Partition]) ->
  io:format("Starting stat vnode~n"),
  {ok, #state {partition = Partition, stats = dict:new()}}.

handle_command({get, StatName}, _Sender, #state{stats=Stats} = State) ->
  Reply = case dict:find(StatName, Stats) of
    error -> not_found;
    Found -> Found
  end,
  {reply, Reply, State};
handle_command({set, StatName, Val}, _Sender, #state{stats=Stats0} = State) ->
  Stats = dict:store(StatName, Val, Stats0),
  {reply, ok, State#state{stats=Stats}};
handle_command({incr, StatName}, _Sender, #state{stats=Stats0} = State) ->
  Stats = dict:update_counter(StatName, 1, Stats0),
  {reply, ok, State#state{stats = Stats}};
handle_command({incrby, StatName, Val}, _Sender, #state{stats=Stats0} = State) ->
  Stats = dict:update_counter(StatName, Val, Stats0),
  {reply, ok, State#state{stats = Stats}};
handle_command({append, StatName, Val}, _Sender, #state{stats=Stats0} = State) ->
  Stats = try dict:append(StatName, Val, Stats0)
    catch _:_ -> dict:store(StatName, [Val], Stats0)
  end,
  {reply, ok, State#state{stats = Stats}};
handle_command({sadd, StatName, Val}, _Sender, #state{stats=Stats0} = State) ->
  F = fun(S) ->
      sets:add_element(Val, S)
  end,
  Stats = dict:update(StatName, F, sets:from_list([Val]), Stats0),
  {reply, ok, State#state{stats=Stats}}.

is_empty(State) ->
  case dict:size(State#state.stats) of
    0 -> {true, State};
    _ -> {false, State}
  end.

encode_handoff_item(StatName, Val) ->
  term_to_binary({StatName, Val}).

handle_handoff_data(Data, #state{stats=Stats0}=State) ->
  {StatName, Val} = binary_to_term(Data),
  Stats = dict:store(StatName, Val, Stats0),
  {reply, ok, State#state{stats=Stats}}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
  Acc = dict:fold(Fun, Acc0, State#state.stats),
  {reply, Acc, State}.

terminate(_Reason, _State) ->
  ok.

delete(State) ->
  {ok, State}.

handle_coverage(_,_,_, State) ->
  {stop, not_implemented, State}.

handle_exit(_, _, State) ->
  {noreply, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_, State) ->
  {ok, State}.

handoff_starting(_, State) ->
  {true, State}.

