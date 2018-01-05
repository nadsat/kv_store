-module(kv_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
  register(kv_server,self()),
  {ok, [], hibernate}.

%% Creates new table
handle_call(start, _From, State) ->
  Tid = ets:new(kvstore,[]),  
  {reply,Tid, [Tid|State],hibernate};

handle_call({store,Tid,K,V}, _From, State) ->
  ets:insert(Tid,{K,V}),
  {reply,ok, State, hibernate};

handle_call({fetch,Tid,K}, _From, State) ->
  Ret = fetch(Tid,K),
  {reply,Ret, State, hibernate};

handle_call({delete,Tid}, _From, State) ->
  ets:delete_all_objects(Tid),
  {reply,ok, State, hibernate};

handle_call({delete,Tid,K}, _From, State) ->
  ets:delete(Tid,K),
  {reply,ok, State, hibernate};

handle_call({all_keys,Tid}, _From, State) ->
  All = ets:tab2list(Tid),
  {reply,All, State, hibernate};

handle_call({lookup,Tid,V}, _From, State) ->
  Ret = lists:flatten(ets:match(Tid,{'$1',V})),
  {reply,Ret, State, hibernate};

handle_call({update, Tid, K, F}, _From, State) ->
  Ret = update( fetch(Tid,K), Tid, K, F ),
  {reply,Ret, State, hibernate};

handle_call({stop,Tid}, _From, State) ->
  NewState = lists:delete(Tid,State),
  {reply,ok, NewState, hibernate}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Data ) ->
    {ok, State}.


%% Internal functions

fetch (Tid,K) ->
 case ets:lookup(Tid,K) of 
   [] -> {error, not_found};
   [{_, V}|_] -> {ok,V}
 end.

update ({ok,V} , Tid, K, F)->
  ets:update_element(Tid, K, [{2,F(V)}]),
  {ok,V};
update (B,_T,_K,_F)->
  B.
