-module(mfvn_wm_entry).
-export([
    init/1,
    allowed_methods/2,
    process_post/2
  ]).

-include_lib("webmachine/include/webmachine.hrl").

init(Config) ->
  {{trace, "/tmp"}, Config}.

allowed_methods(RD, Ctx) ->
  {['POST'], RD, Ctx}.

process_post(RD, Ctx) ->
  case wrq:get_req_header("content-type", RD) of
    "text/plain" ->
      Client = wrq:path_info(client, RD),
      Entry = binary_to_list(wrq:req_body(RD)),
      ok = mfvn:entry(Client, Entry),
      {true, RD, Ctx};
    _ ->
      {{halt, 415}, RD, Ctx}
  end.

