%%%-------------------------------------------------------------------
%% @doc Console Erlang DeBugger public API
%% @end
%%%-------------------------------------------------------------------

-module(cedb).

-export([
  all_breaks/0,
  all_breaks/1,
  debug/2,
  delete_break/2,
  delete_break/3,
  break/2,
  break/3
]).

%%====================================================================
%% API
%%====================================================================

debug(Pid, Srv) ->
  % change group leader to be able to read from user input
  set_shell_group_leader(),
  % attach to the process to debug
  {ok, Meta} = int:attached(Pid),
  % configure meta and pid
  gen_server:cast(Srv, {set, meta, Meta}),
  gen_server:cast(Srv, {set, pid, Pid}),
  % show messages from int server
  messages(Srv),
  % start evaluating user input
  eval(go, Srv).

break(Module, Line) ->
  Pid = cedb_srv,
  gen_server:call(
    Pid, {break, Module, Line, {cedb, debug, [Pid]}}).
break(Module, Function, Arity) ->
  Pid = cedb_srv,
  gen_server:call(
    Pid, {break, {Module, Function, Arity}, {cedb, debug, [Pid]}}).

all_breaks(Module) ->
  int:all_breaks(Module).

all_breaks() ->
  int:all_breaks().

delete_break(Module, Line) ->
  int:delete_break(Module, Line).
delete_break(Module, Function, Arity) ->
  int:delete_break_in(Module, Function, Arity).

%% Private functions

set_shell_group_leader() ->
  case shell:whereis_evaluator() of
    Pid when is_pid(Pid) ->
      ShellInfo = erlang:process_info(Pid),
      GroupLeader = proplists:get_value(group_leader, ShellInfo),
      group_leader(GroupLeader, self());
    Rest ->
      error_logger:info_msg("no pid shell available: ~p~n", [Rest])
  end.

eval(continue, Srv) ->
  gen_server:call(Srv, continue);
eval(finish, _Srv) ->
  ok;
eval(go, Srv) ->
  repl(Srv);
eval(Cmd, Srv) when is_atom(Cmd) ->
  CmdStr = atom_to_binary(Cmd, utf8),
  Cmds = [
           <<"backtrace">>,
           <<"bindings">>,
           <<"continue">>,
           <<"finish">>,
           <<"messages">>,
           <<"next">>,
           <<"skip">>,
           <<"step">>,
           <<"stop">>,
           <<"timeout">>
          ],

  % Get possible commands from abbreviation
  CmdOpts = lists:filter(
              fun(Elem) ->
                case string:prefix(Elem, CmdStr) of
                  nomatch -> false;
                  _       -> true
                end
              end,
              Cmds
            ),

  % Handle command
  Ret = case length(CmdOpts) of
    1 ->
      CmdAtom = erlang:binary_to_atom(lists:nth(1, CmdOpts), utf8),
      case CmdAtom of
        finish -> finish;
        _      ->
        print(CmdAtom, gen_server:call(Srv, CmdAtom)),
        messages(Srv)
      end;
    0 ->
      io:format("Unknown command ~s~n", [Cmd]),
      ok;
    _ ->
      io:format("Bad prefix abbreviation, try one of: ~s~n", [lists:join(", ", CmdOpts)]),
      ok
  end,

  % exit when finished
  case Ret of
    finish -> ok;
    ok     ->
      repl(Srv)
  end;

eval({binding, Var}, Srv) ->
  print(binding, gen_server:call(Srv, {binding, Var})),
  repl(Srv);
eval(Cmd, Srv) ->
  io:format("Unknown command ~p~n", [Cmd]),
  repl(Srv).

repl(Srv) ->
  eval(run(io:get_line("cedb> ")), Srv).

print(bindings, Bindings) ->
  io:format("Bindings ~p~n", [Bindings]);
print(backtrace, Backtrace) ->
  backtrace(Backtrace);
print(_Cmd, ToPrint) ->
  io:format("~p~n", [ToPrint]).

messages(Srv) ->
  Result = receive
    {_, idle} ->
      gen_server:call(Srv, continue);
    {_, {break_at, Module, Line, _}} ->
      {Filename, Content} = gen_server:call(Srv, {get, module, Module}),
      show_line(Filename, Content, Line);
    Msg ->
      io:format("=> ~p~n~n", [Msg])
    after 200 -> exit
  end,
  case Result of
    exit -> ok;
    _ -> messages(Srv)
  end.

show_line(Filename, Content, LineNumber) ->
  Lines = re:split(Content, "\n"),
  {First, Last} = range_to_show(Lines, LineNumber),
  LinesToShow = lists:sublist(Lines, First, Last - First + 1),
  Seq = lists:seq(First, Last),
  io:format("File ~s~n~n", [color:true("108040", Filename)]),
  lists:foreach(fun({Number, Line}) ->
      String = binary_to_list(Line),
      Color = case Number =:= LineNumber of
        true -> "C0C000";
        false -> "606000"
      end,
      io:format("   ~s~n", [color:true(Color, String)])
    end, lists:zip(Seq, LinesToShow)),
  io:format("~n").

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
      "\n";
    error:{unbound_var, Var} ->
      {binding, Var}
  end.

backtrace(Backtrace) ->
  io:format("~n"),
  lists:foreach(fun({Deep, {M,F,A}}) ->
      Space = lists:flatten(lists:duplicate(Deep, " ")),
      io:format("    ~s~s:~s ~p~n", [
        Space,
        color:true("229070", atom_to_list(M)),
        color:true("22A0A0", atom_to_list(F)),
        A
      ])
    end, lists:reverse(Backtrace)),
  io:format("~n").
