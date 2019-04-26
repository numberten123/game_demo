%%%-------------------------------------------------------------------
%% @doc game public API
%% @end
%%%-------------------------------------------------------------------

-module(game_app).

-include("common.hrl").
-include("tcp.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case game_sup:start_link() of
    	{ok, Pid} ->
    		do_init_server(),	%%初始化相关服务
    		{ok, Pid};
    	{error, _Reason} ->
			lager:error("start error, _Reason:~p~n", [_Reason]),
			exit(1)
	end.

%%--------------------------------------------------------------------
stop(_State) ->
	save_mnesia(),
	del_node_info(),
	timer:sleep(2000),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
do_init_server() ->
	init_mnesia(),		%%初始化mnesia
	init_webserver(),	%%初始化web服务
	init_ets(),			%%初始化 ets
	add_node_info(),	%%增加结点信息
	init_tcp_server(),	%%初始化tcp线程池
	data_upgrade:check_upgrade(),
	ok.

init_webserver() ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", web_socket, []},
            {"/[:what]", web_server, []}
		]}
	]),
    {ok, Port} = application:get_env(game, web_port),
	{ok, _} = cowboy:start_clear(web_server, [{port, Port}, {num_acceptors, 16}], #{
		env => #{dispatch => Dispatch}
	}),
    ok.

init_mnesia() ->
	CurNode = node(),
	case application:get_env(game, first_node) of
		{ok, CurNode} ->%%为第一节点，初始化mnesia
			do_init_mnesia(CurNode);
		{ok, FNode} ->
			pong = net_adm:ping(FNode),		%%必须要连上第一结点
			connect_first_node(CurNode, FNode)
	end.

init_ets() ->
	{ok, Path} = application:get_env(game, dets_path),
    FileName = fun(Name) ->
    	Path ++ atom_to_list(Name) ++ ".dets"
    end,
    {ok, _} = dets:open_file(?DATA_DETS_KV, [{file, FileName(?DATA_DETS_KV)}, {keypos, #dets_kv.key}]),

    %% ets
    ets:new(?DATA_DETS_KV,[{keypos, #dets_kv.key}, named_table, public, set]),
	ok.

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

%%落地需要落地的mnesia
save_mnesia() ->
	CurNode = node(),
	case application:get_env(game, first_node) of
		{ok, CurNode} ->%%第一节点
			mnesia:dump_tables([?DATA_MNESIA_KV]);
		_ ->
			ignore
	end.

%%增加节点信息
add_node_info() ->
	{ok, GamePort} = application:get_env(game, game_port),
	{ok, ServerIP} = application:get_env(game, server_ip),
	Fun = fun() ->
		timer:sleep(2000),	%%延迟发送
		global_mnesia_server:update_mnesia_value(?DATA_NODE_INFO, {add_node_info, node(), ServerIP, GamePort})
	end,
	erlang:spawn(Fun).
	
%%删除节点信息
del_node_info() ->
	global_mnesia_server:update_mnesia_value(?DATA_NODE_INFO, {del_node_info, node()}).

%%初始化tcp
init_tcp_server() ->
	{ok, GamePort} = application:get_env(game, game_port),
	{ok, LoginPort} = application:get_env(game, login_port),
	{ok, _} = ranch:start_listener(?TCP_TYPE_GAME, 50, ranch_tcp, [{port, GamePort}, {max_connections, infinity}, {keepalive, true}], tcp_server, [{type, ?TCP_TYPE_GAME}]),
	{ok, _} = ranch:start_listener(?TCP_TYPE_LOGIN, 50, ranch_tcp, [{port, LoginPort}, {max_connections, infinity}, {keepalive, true}], tcp_server, [{type, ?TCP_TYPE_LOGIN}]),
	ok.