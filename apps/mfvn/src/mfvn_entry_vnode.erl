-module(mfvn_entry_vnode).
-behaviour(riak_core_vnode).
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
    is_empty/1,
    start_vnode/1,
    entry/3
  ]).

-export([combined_lf/2]).

-define(MASTER, mfvn_entry_vnode_master).
-define(NGINX_LOG_FORMAT, "^(\\S*) (\\S*) (\\S*) ").

-record(state, {partition, reg}).

start_vnode(I) ->
  io:format("entry - start_vnode~n"),
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

entry(IdxNode, Client, Entry) ->
  riak_core_vnode_master:command(IdxNode, {entry, Client, Entry}, ?MASTER).

init([Partition]) ->
  Reg = [
    {?NGINX_LOG_FORMAT, fun ?MODULE:combined_lf/2}
  ],
  {ok, #state {partition = Partition, reg=Reg}}.

handle_command({entry, Client, Entry}, _Sender, #state{reg=Reg}=State) ->
  lists:foreach(match(Client, Entry), Reg),
  {noreply, State}.

match(Client, Entry) ->
  fun({Regexp, Fun}) ->
      case re:run(Entry, Regexp, [{capture, all, list}]) of
          nomatch ->
            ignore;
          {match, Match} ->
            Fun({Client, Entry, Regexp}, Match)
        end
    end.

terminate(_Reason, _State) ->
  ok.

delete(State) ->
  {ok, State}.

encode_handoff_item(_StatName, _Val) ->
  <<>>.

handle_coverage(_,_,_, State) ->
  {stop, not_implemented, State}.

handle_exit(_, _, State) ->
  {noreply, State}.

handle_handoff_command(_,_, State) ->
  {noreply, State}.

handle_handoff_data(_, State) ->
  {reply, ok, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_, State) ->
  {ok, State}.

handoff_starting(_, State) ->
  {true, State}.

is_empty(State) ->
  {true, State}.

combined_lf(
  {Client, _Entry, _Regexp},
  [_FullString, _IP, _, DatabaseID]
) ->
  mfvn:sadd(Client, "databases", DatabaseID),
  mfvn:incr(Client, DatabaseID).
