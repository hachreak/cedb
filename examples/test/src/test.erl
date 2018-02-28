% Example module to debug.
%

-module(test).

-export([
  start/0
]).

start() ->
  function1(),
  io:format("Finish~n").

function1() ->
  A = "Hello",
  B = #{A => <<"c">>},
  function2(B),
  io:format("I'm in function 1~n").

function2(C) ->
  io:format("I'm in function 2, the value is ~p~n", [C]),
  io:format("end function 2~n").

