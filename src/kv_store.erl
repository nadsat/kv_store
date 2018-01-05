%% 
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @doc Simple KV store 

-module (kv_store).

%%API

-export ([start/0,store/3,fetch/2,delete/1,delete/2,all_keys/1,lookup/2,update/3,stop/1]).

-type reason() :: term().
-type key() :: term().
-type value() :: term().
-type store() :: ets:tid().

%% @doc Starts a new kv store
-spec start() -> {ok, store()} | {error, reason()}.

start()->
  start_ok(application:start(kv_store)).

start_ok (ok)->
  Tid = gen_server:call(kv_server,start),
  {ok,Tid};
start_ok({error, {already_started, _App}}) ->
  Tid = gen_server:call(kv_server,start),
  {ok,Tid};
start_ok({error, Reason}) ->
  {error, Reason}.

%% @doc Stores a value in a kv store
-spec store(store(), key(), value()) -> ok.
store (Tid,K,V) ->
  gen_server:call(kv_server,{store,Tid,K,V}).

%% @doc Retrieves a value from a kv store
-spec fetch(store(), key()) -> {ok, value()} | {error, not_found}.
fetch (Tid,K) ->
  gen_server:call(kv_server,{fetch,Tid,K}).

%% @doc Removes an item from a kv store.
%% If the key had no value associated, nothing happens.
-spec delete(store(), key()) -> ok.
delete (Tid,K) ->
  gen_server:call(kv_server,{delete,Tid,K}).

%% @doc Removes all items from a kv store.
-spec delete(store()) -> ok.
delete (Tid) ->
  gen_server:call(kv_server,{delete,Tid}).

%% @doc Returns the list of all defined keys in a kv store.
-spec all_keys(store()) -> [key()].
all_keys (Tid) ->
  gen_server:call(kv_server,{all_keys,Tid}).

%% @doc Finds all the keys with an associated value within a kv store.
-spec lookup(store(), value()) -> [key()].
lookup (Tid,V) ->
  gen_server:call(kv_server,{lookup,Tid,V}).

%% @doc Updates the value of an item from a kv store.
%% If the key had no value associated, an error is returned.
%% Returns the previous value.
-spec update(store(), key(), fun( (value()) -> value() )) ->
                  {ok, value()} | {error, not_found}.
update (Tid,K,F) ->
  gen_server:call(kv_server,{lookup,Tid,K,F}).

%% @doc Stops a kv store.
-spec stop(store()) -> ok.
stop (Tid) ->
  gen_server:call(kv_server,{stop,Tid}).
