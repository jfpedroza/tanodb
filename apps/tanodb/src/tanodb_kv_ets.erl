-module(tanodb_kv_ets).
-export([new/1, get/3, put/4, delete/3]).

-record(state, {table_id}).

new(_Opts) ->
    TableId = ets:new(?MODULE, [set, {write_concurrency, false},
                                {read_concurrency, false}]),
    State = #state{table_id=TableId},
    {ok, State}.

put(State=#state{table_id=TableId}, Bucket, Key, Value) ->
    K = {Bucket, Key},
    true = ets:insert(TableId, {K, Value}),
    {ok, State}.

get(State=#state{table_id=TableId}, Bucket, Key) ->
    K = {Bucket, Key},
    Res = case ets:lookup(TableId, K) of
              [] -> {not_found, K};
              [{_, Value}] -> {found, {K, Value}}
          end,
    {Res, State}.

delete(State=#state{table_id=TableId}, Bucket, Key) ->
    K = {Bucket, Key},
    true = ets:delete(TableId, K),
    {ok, State}.

