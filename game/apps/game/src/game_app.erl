%%%-------------------------------------------------------------------
%% @doc game public API
%% @end
%%%-------------------------------------------------------------------

-module(game_app).

-include("common.hrl").
-include("tcp.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, do_stop/1]).

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
stop(State) ->
	do_stop(State),
	timer:sleep(2000),
    ok.

do_stop(_) ->
	init_mnesia:save(),
	init_ets:save(),
	del_node_info().
%%====================================================================
%% Internal functions
%%====================================================================
do_init_server() ->
	init_mnesia:init(),		%%初始化mnesia
	init_ets:init(),		%%初始化 ets
	init_webserver(),		%%初始化web服务
	add_node_info(),		%%增加结点信息
	data_upgrade:check_upgrade(),	%%数据升级
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