-module(mfvn_write_fsm).
-bahavior(gen_fsm).
-include("mfvn.hrl").

-export([
    init/1,
    prepare/2,
    execute/2,
    waiting/2,
    start_link/5,
    start_link/6,
    write/3,
    write/4,
    terminate/3
  ]).

-record(state, {req_id, preflist, from, client, stat_name, op, val, num_w=0}).

start_link(ReqId, From, Client, StatName, Op) ->
  start_link(ReqId, From, Client, StatName, Op, undefined).

start_link(ReqID, From, Client, StatName, Op, Val) ->
  gen_fsm:start_link(?MODULE, [ReqID, From, Client, StatName, Op, Val], []).

write(Client, StatName, Op) ->
  write(Client, StatName, Op, undefined).

write(Client, StatName, Op, Val) ->
  ReqID = make_request_id(),
  mfvn_write_fsm_sup:start_write_fsm([ReqID, self(), Client, StatName, Op, Val]),
  {ok, ReqID}.

init([ReqId, From, Client, StatName, Op, Val]) ->
  SD = #state{
    req_id=ReqId,
    from=From,
    client=Client,
    stat_name=StatName,
    op=Op,
    val=Val
  },
  {ok, prepare, SD, 0}.

prepare(timeout, SD0=#state{client=Client, stat_name=StatName}) ->
  DocIdx = riak_core_util:chash_key({
      list_to_binary(Client),
      list_to_binary(StatName)
    }),
  PreList = riak_core_apl:get_apl(DocIdx, ?N, mfvn_stat),
  io:format("Going to write to: ~p~n", [PreList]),
  SD = SD0#state{preflist=PreList},
  {next_state, execute, SD, 0}.

execute(timeout, SD0=#state{
    req_id=ReqId,
    stat_name=StatName,
    preflist=Prelist,
    op=Op,
    val=Val
  }) ->
  case Val of
    undefined -> mfvn_stat_vnode:Op(Prelist, ReqId, StatName);
    _ -> mfvn_stat_vnode:Op(Prelist, ReqId, StatName, Val)
  end,
  {next_state, waiting, SD0}.

terminate(_,_,_) ->
  ok.

waiting({ok, ReqId}, SD0=#state{from=From, num_w=NumW0}) ->
  NumW = NumW0 + 1,
  SD = SD0#state{num_w=NumW},
  if
    NumW =:= ?W ->
      From ! {ReqId, ok},
      {stop, normal, SD};
    true -> {next_state, waiting, SD}
  end.

make_request_id() -> erlang:phash2(erlang:now()).
