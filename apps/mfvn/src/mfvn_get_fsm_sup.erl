-module(mfvn_get_fsm_sup).
-behavior(supervisor).

-export([
    start_get_fsm/1,
    start_link/0,
    init/1
  ]).

start_get_fsm(Args) ->
  supervisor:start_child(?MODULE, Args).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  GetFSM = {
    undefined,
    {mfvn_get_fsm, start_link, []},
    temporary,
    5000,
    worker,
    [mfvn_get_fsm]
  },
  {ok, {{simple_one_for_one, 10, 10}, [GetFSM]}}.

