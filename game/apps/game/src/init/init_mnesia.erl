-module(init_mnesia).

-include("common.hrl").
-export([init/0, save/0]).

%%初始化
init() ->
	CurNode = node(),
	case application:get_env(game, first_node) of
		{ok, CurNode} ->%%为第一节点，初始化mnesia
			do_init_mnesia(CurNode);
		{ok, FNode} ->
			pong = net_adm:ping(FNode),		%%必须要连上第一结点
			connect_first_node(CurNode, FNode)
	end.

%%落地的mnesia
save() ->
	CurNode = node(),
	case application:get_env(game, first_node) of
		{ok, CurNode} ->%%第一节点
			mnesia:dump_tables([?DATA_MNESIA_KV]);
		_ ->
			ignore
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%初始化mnesia
do_init_mnesia(CurNode) ->
	mnesia:stop(),
	mnesia:create_schema([CurNode]),
	mnesia:start(),
	mnesia:create_table(?DATA_MNESIA_KV, [{ram_copies, [CurNode]}, {attributes, record_info(fields, mnesia_kv)}]),
	mnesia:create_table(?DATA_NODE_INFO, [{ram_copies, [CurNode]}, {attributes, record_info(fields, mnesia_node_info)}]),
	ok.

%%联接到第一节点
connect_first_node(CurNode, FNode) ->
	mnesia:stop(),
	mnesia:delete_schema([CurNode]),
	mnesia:start(),
	mnesia:change_config(extra_db_nodes, [FNode]),
	mnesia:change_table_copy_type(schema, CurNode, disc_copies),
	lists:foreach(fun(Table) ->
		Type = rpc:call(FNode, mnesia, table_info, [Table, storage_type]),
		mnesia:add_table_copy(Table, CurNode, Type)
	end, mnesia:system_info(tables)--[schema]).