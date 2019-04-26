-module(init_ets).

-include("common.hrl").
-export([init/0, save/0]).

%%初始化
init() ->
    {ok, Path} = application:get_env(game, dets_path),
    FileName = fun(Name) ->
        Path ++ atom_to_list(Name) ++ ".dets"
    end,
    {ok, _} = dets:open_file(?DATA_DETS_KV, [{file, FileName(?DATA_DETS_KV)}, {keypos, #dets_kv.key}]),

    %% ets
    ets:new(?DATA_DETS_KV,[{keypos, #dets_kv.key}, named_table, public, set]),

    %%部分dets转ets，读取更快
    dets_to_ets().

%%落地dets
save() ->
    dets:from_ets(?DATA_DETS_KV, ?DATA_DETS_KV).
%%%===================================================================
%%% Internal functions
%%%===================================================================
dets_to_ets() ->
    ets:from_dets(?DATA_DETS_KV, ?DATA_DETS_KV).