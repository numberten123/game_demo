#!/usr/bin/env escript
%% -*- erlang -*-
%%! -hidden
%% 配置 server_info_list,node_name

main(ModuleList) ->
	io:setopts([{encoding, unicode}]),
	application:set_env(kernel, dist_auto_connect, never), %% Hidden node
	{ok, _} = net_kernel:start([node_name(), longnames]),
	NodeInfoList = server_info_list(),
	[begin
		 erlang:set_cookie(node(), Cookie),
		 net_kernel:hidden_connect_node(Node),
		 send_and_swap_code(Node, ModuleList)
	 end|| {Node, Cookie} <- NodeInfoList].

send_and_swap_code(Node, _ModuleList) ->
	try
		rpc:call(Node, game_app, do_stop, [1])
	catch
		Error:Reason ->
			io:format("stacktrace:~s, Error:~p, Reason:~p~n" , [erlang:get_stacktrace(), Error, Reason])
	end.


%% 节点名称， 节点cookie
server_info_list() ->
	[
	 {'game_api@10.168.100.52', 'game_cookie'}
	].

%% 这里配置启动节点名称, host改为对应的内网ip
node_name() ->
	'game_remote@10.168.100.52'.