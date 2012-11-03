-module(mfvn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
  io:format("mfvn_sup:init~n"),
    VMaster = { mfvn_vnode_master,
                  {riak_core_vnode_master, start_link, [mfvn_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},
    Entry = { mfvn_entry_vnode_master,
                  {riak_core_vnode_master, start_link, [mfvn_entry_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},
    Stat = { mfvn_stat_vnode_master,
                  {riak_core_vnode_master, start_link, [mfvn_stat_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},

    WriteFSMs = { mfvn_write_fsm_sup,
                  {mfvn_write_fsm_sup, start_link, []},
                  permanent, infinity, supervisor, [mfvn_write_fsm_sup]},
    GetFSMs = { mfvn_get_fsm_sup,
                  {mfvn_get_fsm_sup, start_link, []},
                  permanent, infinity, supervisor, [mfvn_get_fsm_sup]},

    { ok,
        { {one_for_one, 5, 10},
          [VMaster, Entry, Stat, WriteFSMs, GetFSMs]}}.
