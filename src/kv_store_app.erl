%%%-------------------------------------------------------------------
%% @doc kv_store public API
%% @end
%%%-------------------------------------------------------------------

-module(kv_store_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    kv_store_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
