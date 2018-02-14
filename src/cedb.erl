%%%-------------------------------------------------------------------
%% @doc Console Erlang DeBugger public API
%% @end
%%%-------------------------------------------------------------------

-module(cedb).

-export([
  debug/1,
  break/2
]).

%%====================================================================
%% API
%%====================================================================

break(Module, Line) ->
  int:i(Module),
  int:break(Module, Line),
  int:auto_attach([break], {?MODULE, debug, []}).

debug(Pid) ->
  {ok, Meta} = int:attached(Pid),
  eval(go, Meta),
  int:continue(Pid),
  false.

eval(continue, _) -> ok;
eval(Cmd, Meta) ->
  case lists:member(Cmd, [finish, next, step, skip, stop,
                          messages, timeout]) of
    true ->
      int:meta(Meta, Cmd);
    false ->
      eval2(Cmd, Meta)
  end,
  repl(Meta).

eval2(bindings, Meta) ->
  Ret = int:meta(Meta, bindings, nostack),
  io:format("bind: ~p~n", [Ret]);
eval2(_, _) -> ok.

repl(Meta) ->
  eval(run(io:get_line("cedb> ")), Meta).

run("\n") -> "";
run(Expression) ->
  % io:format("expr: (~p)~n", [Expression]),
  % scan the code into tokens
  {ok, Tokens, _} = erl_scan:string(Expression),
  % parse the tokens into an abstract form
  {ok, Parsed} = erl_parse:parse_exprs(Tokens),
  % evaluate the expression, return the value
  {value, Result, _} = erl_eval:exprs(Parsed, []),
  Result.
