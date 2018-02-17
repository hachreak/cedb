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
  meta_response(),
  eval(go, Meta),
  int:continue(Pid),
  false.

eval(continue, _) -> ok;
eval(Cmd, Meta) ->
  case lists:member(Cmd, [finish, next, step, skip, stop,
                          messages, timeout]) of
    true ->
      int:meta(Meta, Cmd),
      meta_response();
    false ->
      eval2(Cmd, Meta)
  end,
  repl(Meta).

eval2(bindings, Meta) ->
  Ret = int:meta(Meta, bindings, nostack),
  io:format("bind: ~p~n", [Ret]),
  meta_response();
eval2(_, _) -> ok.

repl(Meta) ->
  eval(run(io:get_line("cedb> ")), Meta).

meta_response() ->
  receive
    {_Pid, {attached, Module, Line, _Trace}} ->
      show_line(Module, Line);
    {_Pid, {break_at, Module, Line, _Column}} ->
      show_line(Module, Line);
    Msg -> io:format("MSG: ~p~n", [Msg])
  end.

show_line(Module, LineNumber) ->
  Filename = int:file(Module),
  {ok, File} = file:read_file(Filename),
  Lines = re:split(File, "\n"),
  {First, Last} = range_to_show(Lines, LineNumber),
  LinesToShow = lists:sublist(Lines, First, Last - First + 1),
  Seq = lists:seq(First, Last),
  lists:foreach(fun({Number, Line}) ->
      String = binary_to_list(Line),
      Color = case Number =:= LineNumber of
        true -> "C0C000";
        false -> "606000"
      end,
      io:format("  ~p:\t~s~n", [Number, color:true(Color, String)])
    end, lists:zip(Seq, LinesToShow)).

range_to_show(Lines, LineNumber) ->
  Length = length(Lines),
  Begin = LineNumber - 5,
  End = LineNumber + 5,
  Last = case End > Length of
    true -> Length;
    false -> End
  end,
  First = case Begin < 0 of
    true -> 0;
    false -> Begin
  end,
  {First, Last}.

run("\n") -> "";
run(Expression) ->
  try
    % scan the code into tokens
    {ok, Tokens, _} = erl_scan:string(Expression),
    % parse the tokens into an abstract form
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    % evaluate the expression, return the value
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    Result
  catch
    error:{badmatch, {error, {_, erl_parse, Err}}} ->
      io:format("[~s] ~p~n", [color:red("error"), Err]),
      "\n"
  end.
