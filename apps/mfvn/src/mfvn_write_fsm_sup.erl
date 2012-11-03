-module(mfvn_write_fsm_sup).
-behavior(supervisor).

-export([
    start_write_fsm/1,
    start_link/0,
    init/1
  ]).

start_write_fsm(Args) ->
  supervisor:start_child(?MODULE, Args).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  WriteFSM = {
    undefined,
    {mfvn_write_fsm, start_link, []},
    temporary,
    5000,
    worker,
    [mfvn_write_fsm]
  },
  {ok, {{simple_one_for_one, 10, 10}, [WriteFSM]}}.

