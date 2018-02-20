%%%-------------------------------------------------------------------
%% @doc Backend server
%% @end
%%%-------------------------------------------------------------------

-module(cedb_srv).

-behaviour(gen_server).

-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_info/2,
  handle_cast/2
]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
  gen_server:start_link(?MODULE, [#{}], []).

init([Ctx]) ->
  {ok, Ctx}.

handle_call(Msg, _, Ctx) ->
  {Reply, Ctx2} = sync_cmd(Msg, Ctx),
  {reply, Reply, Ctx2}.

handle_cast(Msg, Ctx) ->
  Ctx2 = async_cmd(Msg, Ctx),
  {noreply, Ctx2}.

handle_info(Msg, Ctx) ->
  error_logger:info_msg("Msg not catched: ~p~n", [Msg]),
  {noreply, Ctx}.

%% Backend

async_cmd({set, pid, Pid}, Ctx) ->
  Ctx#{pid => Pid};
async_cmd({set, meta, Meta}, Ctx) ->
  Ctx#{meta => Meta}.

sync_cmd({get, module, Module}, #{pid := Pid}=Ctx) ->
  Filename = int:file(Module),
  Content = int:contents(Module, Pid),
  {{Filename, Content}, Ctx};
sync_cmd({get, pid}, #{pid := Pid}=Ctx) ->
  {Pid, Ctx};
sync_cmd({break, Module, Line, MFA}, Ctx) ->
  int:i(Module),
  int:break(Module, Line),
  int:auto_attach([break], MFA),
  {ok, Ctx};
sync_cmd(continue, #{pid := Pid}=Ctx) ->
  int:continue(Pid),
  {ok, Ctx};
sync_cmd(backtrace, #{meta := Meta}=Ctx) ->
  Ret = int:meta(Meta, backtrace, 100),
  {Ret, Ctx};
sync_cmd(bindings, #{meta := Meta}=Ctx) ->
  Ret = int:meta(Meta, bindings, nostack),
  {Ret, Ctx};
sync_cmd(Msg, #{meta := Meta}=Ctx) ->
  Ret = case lists:member(Msg, [finish, next, step, skip, stop,
                          messages, timeout]) of
    true ->
      int:meta(Meta, Msg);
    false ->
      ok
  end,
  {Ret, Ctx}.
