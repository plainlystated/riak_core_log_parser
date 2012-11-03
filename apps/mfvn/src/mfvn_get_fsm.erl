-module(mfvn_get_fsm).
-bahavior(gen_fsm).
-include("mfvn.hrl").

-export([
    init/1,
    prepare/2,
    execute/2,
    waiting/2,
    start_link/4,
    get/2,
    terminate/3
  ]).

-record(state, {req_id, preflist, from, client, stat_name, num_r=0, op, val, replies=[]}).

start_link(ReqId, From, Client, StatName) ->
  gen_fsm:start_link(?MODULE, [ReqId, From, Client, StatName], []).

get(Client, StatName) ->
  ReqId = make_request_id(),
  mfvn_get_fsm_sup:start_get_fsm([ReqId, self(), Client, StatName]),
  {ok, ReqId}.

init([ReqId, From, Client, StatName]) ->
  SD = #state{req_id=ReqId,
    from=From,
    client=Client,
    stat_name=StatName},
  {ok, prepare, SD, 0}.

prepare(timeout, SD0=#state{client=Client, stat_name=StatName}) ->
  DocIdx = riak_core_util:chash_key({
      list_to_binary(Client),
      list_to_binary(StatName)
    }),
  PreList = riak_core_apl:get_apl(DocIdx, ?N, mfvn_stat),
  io:format("Going to read from: ~p~n", [PreList]),
  SD = SD0#state{preflist=PreList},
  {next_state, execute, SD, 0}.

execute(timeout, SD0=#state{req_id=ReqId, stat_name=StatName, preflist=Prelist}) ->
  mfvn_stat_vnode:get(Prelist, ReqId, StatName),
  {next_state, waiting, SD0}.

terminate(_,_,_) ->
  ok.

waiting({ok, ReqId, Val}, SD0=#state{from=From, num_r=NumR0, replies = Replies0}) ->
  NumR = NumR0 + 1,
  Replies = [Val | Replies0],
  SD = SD0#state{num_r=NumR, replies=Replies},
  if
    NumR =:= ?R ->
      Reply = case lists:any(different(Val), Replies) of
        true -> Replies;
        false -> Val
      end,
      From ! {ReqId, ok, Reply},
      {stop, normal, SD};
    true ->
      {next_state, waiting, SD}
  end.

different(A) -> fun(B) -> A =/= B end.

make_request_id() -> erlang:phash2(erlang:now()).
