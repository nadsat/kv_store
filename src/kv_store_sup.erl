%%%-------------------------------------------------------------------
%% @doc kv_store top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kv_store_sup).

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
    MaxRestart = 5,
    MaxTime = 100,
    {ok, {{one_for_one, MaxRestart, MaxTime},
     [{kv_server,
       {kv_server, start_link, []},
        permanent,
        60000,
        worker,
        [serial_fsm]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
