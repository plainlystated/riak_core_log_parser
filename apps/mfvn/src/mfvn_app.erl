-module(mfvn_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  io:format("starting app~n"),
    case mfvn_sup:start_link() of
        {ok, Pid} ->
          ok = riak_core:register_vnode_module(mfvn_vnode),
          ok = riak_core_node_watcher:service_up(mfvn, self()),

          ok = riak_core:register_vnode_module(mfvn_entry_vnode),
          ok = riak_core_node_watcher:service_up(mfvn_entry, self()),

          ok = riak_core:register_vnode_module(mfvn_stat_vnode),
          ok = riak_core_node_watcher:service_up(mfvn_stat, self()),

          EntryRoute = {["mfvn", "entry", client], mfvn_wm_entry, []},
          webmachine_router:add_route(EntryRoute),
          io:format("app - adding route~n"),

          {ok, Pid};
        {error, Reason} ->
          {error, Reason}
    end.

stop(_State) ->
    ok.
