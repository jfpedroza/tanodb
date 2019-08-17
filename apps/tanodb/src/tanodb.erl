-module(tanodb).

-export([ping/0, get/2, put/3, delete/2]).
-ignore_xref([ping/0, get/2, put/3, delete/2]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    send_to_one(<<"ping">>, term_to_binary(os:timestamp()), ping).

get(Bucket, Key) ->
    ReqId = make_ref(),
    send_to_one(Bucket, Key, {get, ReqId, {Bucket, Key}}).

put(Bucket, Key, Value) ->
    ReqId = make_ref(),
    send_to_one(Bucket, Key, {put, ReqId, {Bucket, Key, Value}}).

delete(Bucket, Key) ->
    ReqId = make_ref(),
    send_to_one(Bucket, Key, {delete, ReqId, {Bucket, Key}}).

send_to_one(Bucket, Key, Cmd) ->
    DocIdx = riak_core_util:chash_key({Bucket, Key}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, tanodb),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd, tanodb_vnode_master).
