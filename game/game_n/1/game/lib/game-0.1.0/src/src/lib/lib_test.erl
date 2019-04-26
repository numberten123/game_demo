-module(lib_test).

-compile(export_all).
-include("common.hrl").

%%%===================================================================
%%% redis, ets, dets, mnesia 读写性能比拼, cfg完胜
%%%===================================================================
%% timer:tc(lib_test, test_redis, [10000]). 2214584
%% timer:tc(lib_test, test_dets, [10000]).	1417724
%% timer:tc(lib_test, test_ets, [10000]).	3952
%% timer:tc(lib_test, test_mnesia, [10000]). 349267
%% 10000：ets>mnesia>dets>redis>mysql，读写 ets比dets快300-400倍，dets插入新key较慢, redis新旧一样，总体dets比redis快2~3倍
%% 读ets是mnesia脏读的4-5倍，写40倍，读写100倍，mnesia脏读效率很高
%% 尽量不使用redis,dets

%% timer:tc(lib_test, test_mysql, [10000]).
test_mysql(0) ->
	ok;
test_mysql(Num) ->
	lib_query:mysql_query(?POOL_GAME, "select id from data_upgrade where status=1"), %% 1968782μs
	test_mysql(Num-1).

% test_redis(0) ->
% 	ok;
% test_redis(Num) ->
% 	lib_query:get_redis_value(Num),					%% 1181811μs
% 	lib_query:set_redis_value(Num,Num),				%% 1466774μs
% 	test_redis(Num-1).

test_dets(0) ->
	ok;
test_dets(Num) ->
	dets:lookup(?DATA_DETS_KV, Num),							%% 619838μs
	dets:insert(?DATA_DETS_KV,#dets_kv{key=Num,value=Num}),		%% 86328μs
	test_dets(Num-1).

test_ets(0) ->
	ok;
test_ets(Num) ->
	ets:lookup(?DATA_DETS_KV, Num),								%% 931μs
	ets:insert(?DATA_DETS_KV,#dets_kv{key=Num,value=Num}),		%% 3455μs
	test_ets(Num-1).

%%事务读写太慢
test_mnesia(0) ->
	ok;
test_mnesia(Num) ->
	%%mnesia:dirty_read(mnesia_kv,Num),
	mnesia:transaction(fun() ->
		mnesia:read(?DATA_MNESIA_KV, Num),
		mnesia:write(#mnesia_kv{key=Num,value=Num})
	end),
	%% 349267μs
	test_mnesia(Num-1). 

%%进程控制，同步与异步差距
test_mnesia2(0) ->
	ok;
test_mnesia2(Num) ->
	lib_query:get_mnesia_value(?DATA_MNESIA_KV, Num),					%% 5742μs
	mnesia:dirty_write(#mnesia_kv{key=Num,value=Num}),					%% 17926μs
	lib_query:call_mnesia_value(?DATA_MNESIA_KV, {get_value, Num}),		%% 26869μs
	test_mnesia2(Num-1).

%%配置表读取速度
test_cfg(0) ->
	ok;
test_cfg(Num) ->
	cfg_test:get_single(Num),		%% 119μs
	test_cfg(Num-1).

%%协议通讯测试
test_tcp(Num) ->
	{ok, Login} = gen_tcp:connect("10.168.100.52", 5253, [binary], 5000),
	{ok, Game} = gen_tcp:connect("10.168.100.52", 5254, [binary], 5000),
	List = lists:seq(1,Num),
	{ok, Bin} = pack(1001,#m_1001_tos{}),
	[gen_tcp:send(Login, Bin)||_ID<-List],
	[gen_tcp:send(Game, Bin)||_ID<-List],
	gen_tcp:close(Login),
	gen_tcp:close(Game),
	ok.

%%获取时间函数测试
%%timer:tc(lib_test, test_time, [10000]). 
test_time(0) ->
	ok;
test_time(Num) ->
	erlang:system_time(seconds),	%%847
	os:timestamp(),			%%400
	test_time(Num-1).

%%timer:tc(lib_test, test_pb, [10000]). 
test_pb(0) ->
	ok;
test_pb(Num) ->
	Pk = #m_1001_tos{},
	{ok, <<_DataSize:16, _Cmd:16, Bin/binary>>} = pack(1001,Pk),
	packet:unpack(1001, Bin),		
	%% 9545
	Bin2 = term_to_binary(Pk),
	binary_to_term(Bin2),
	%%1696
	test_pb(Num-1).

% test_pid2() ->
% 	qwe=asd.

test_pid() ->
	spawn(fun() -> loop() end).

loop() ->
	lager:error("self~p~n",[self()]),
	process_flag(trap_exit, true),
	proc_lib:start_link(lib_test, test_pid2, [], infinity),
	receive
		Msg ->
			lager:error("~p~n",[Msg])
	end,
	loop().
	
%%%===================================================================
%%% Internal functions
%%%===================================================================
pack(Cmd, Data) ->
	String	 = "encode_m_" ++ erlang:integer_to_list(Cmd) ++ "_tos",
	Fun		 = lib_misc:list_to_atom(String),
	PbList	 = all_pb:Fun(Data),
	Bin		 = erlang:iolist_to_binary(PbList),
	DataSize = erlang:byte_size(Bin),
	RawBin	 = <<DataSize:16, Cmd:16, Bin/binary>>,
	{ok, RawBin}.

unpack(Cmd, Bin) ->
	String = "decode_m_" ++ erlang:integer_to_list(Cmd) ++ "_toc",
	Fun	   = lib_misc:list_to_atom(String),
	%% 在版本升级时，服务器的版本会落后客户端
	try all_pb:Fun(Bin) of
		Data ->
			{ok, Cmd, Data}
	catch
		Class:Reason ->
			lager:error("~n error in unpack, Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
			error
	end.