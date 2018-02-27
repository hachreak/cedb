%%%-------------------------------------------------------------------
%% @doc cedb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cedb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Children = [
    {cedb_srv,
     {cedb_srv, start_link, []},
     permanent, 1000, worker, [cedb_srv]
    }
  ],
  {ok, { {one_for_one, 0, 1}, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
