-module (kv_store_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([kv_store_usage/1]).

all() -> [kv_store_usage].

init_per_testcase(kv_store_usage, Config) ->
  {ok,Tid} = kv_store:start(),
  [{table,Tid} | Config].

end_per_testcase(kv_store_usage, Config) ->
  kv_store:stop(?config(table, Config)).

kv_store_usage(Config) ->
  Tid = ?config(table, Config),
  ok = kv_store:store(Tid,antonio,74),
  {ok,74} = kv_store:fetch(Tid,antonio),
  ok = kv_store:store(Tid,pamela,44),
  ok = kv_store:store(Tid,gonzalo,36),
  [{pamela,44},{antonio,74},{gonzalo,36}] = kv_store:all_keys(Tid).
