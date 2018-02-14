%%%-------------------------------------------------------------------
%% @doc Console Erlang DeBugger public API
%% @end
%%%-------------------------------------------------------------------

-module(cedb).

-export([
  debug/1,
  show/1,
  break/2
]).

%%====================================================================
%% API
%%====================================================================

break(Module, Line) ->
  int:i(Module),
  int:break(Module, Line),
  int:auto_attach([break], {?MODULE, debug, []}),
  int:test_at_break(Module, Line, {?MODULE, show}).

debug(Pid) ->
  eval(go, Pid),
  int:continue(Pid),
  false.

show(Bindings) ->
  Print = lists:map(fun({Var, Bind}) ->
      sf:format("{{var}} = {{bind}}", [{var, Var}, {bind, Bind}], [string])
    end, Bindings),
  error_logger:error_msg("~p~n", [lists:flatten(lists:join("~n", Print))]),
  true.

eval(continue, _Pid) -> ok;
eval(Cmd, Pid) ->
  case lists:member(Cmd, [finish, next, step]) of
    true ->
      apply(int, Cmd, [Pid]);
    false -> ok
  end,
  repl(Pid).

repl(Pid) ->
  eval(run(io:get_line("cedb> ")), Pid).

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
