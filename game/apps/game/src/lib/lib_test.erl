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
%% timer:tc(lib_test, test_mysql, [10000]). 1968782
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

%%timer:tc(lib_test, test_dict, [10000]).		392
test_dict(0) ->
	ok;
test_dict(Num) ->
	% put(Num,Num),
	% get(Num),
	test_dict(Num-1).

%%配置表读取速度
test_cfg(0) ->
	ok;
test_cfg(Num) ->
	cfg_test:get_single(Num),		%% 119μs
	test_cfg(Num-1).

%%协议通讯测试
% test_tcp(Num) ->
% 	{ok, Login} = gen_tcp:connect("10.168.100.52", 5253, [binary], 5000),
% 	{ok, Game} = gen_tcp:connect("10.168.100.52", 5254, [binary], 5000),
% 	List = lists:seq(1,Num),
% 	{ok, Bin} = packet:pack(1001,#m_1001_tos{}),
% 	[gen_tcp:send(Login, Bin)||_ID<-List],
% 	[gen_tcp:send(Game, Bin)||_ID<-List],
% 	gen_tcp:close(Login),
% 	gen_tcp:close(Game),
% 	ok.

%%获取时间函数测试
%%timer:tc(lib_test, test_time, [10000]). 
test_time(0) ->
	ok;
test_time(Num) ->
	erlang:system_time(seconds),	%%847
	os:timestamp(),			%%400
	test_time(Num-1).

%%timer:tc(lib_test, test_pb, [10000]). 
% test_pb(0) ->
% 	ok;
% test_pb(Num) ->
% 	Pk = #m_1004_toc{
% 		ip="qwe",
% 		role_id=123,
% 		test_info = [#p_test{ip="asd",role_id=234}, #p_test{ip="asd",role_id=234}, #p_test{ip="asd",role_id=234}]
% 	},
% 	% {ok, <<_DataSize:16, _Cmd:16, Bin/binary>>} = packet:pack(1004,Pk),
% 	% unpack(1004, Bin),		
% 	%% 132826
% 	% Bin2 = term_to_binary(Pk),
% 	% binary_to_term(Bin2),
% 	%% 5634
% 	Bin3 = packet:pack_pb(Pk),
% 	packet:unpack_pb(m_1004_toc, Bin3),
% 	%%42747
% 	% String = "m_" ++ erlang:integer_to_list(9999) ++ "_tos",
% 	% lib_misc:list_to_atom(String),
% 	%%1700
% 	test_pb(Num-1).

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

test_rand(0) ->
	ok;
test_rand(Num) ->
	%%rand:uniform(),		%%1818
	rand:uniform(1000000000000000),	%%2156
	test_rand(Num-1).

test_json(0) ->
	ok;
test_json(Num) ->
	List = [{list,[[{id,1},{rate,0.973},{bdxs,0.808}],
        [{id,2},{rate,0.967},{bdxs,0.97}],
        [{id,3},{rate,0},{bdxs,1.111}]]}],
    %%jsx:encode(List),	%%311689
    iolist_to_binary(mochijson2:encode(List)),	%%76016
	test_json(Num-1).

test_io(0) ->
	ok;
test_io(Num) ->
	io:format("~w~n",[Num]),
	test_io(Num-1).

%%timer:tc(lib_test, test_struct, [10000,#{}]). 
%%只做set和get maps比lists快将近10倍， 只set和遍历，lists比maps快10倍以上, lists用列表推导则更快
test_struct(0,_R) ->
	%%Fun = fun({K,V}) -> K+1,V+1 end,		lists:foreach(Fun,R),	%%[ID||ID<-R],		%% 523
	%%Fun = fun(K,V) -> K+1,V+1 end,		maps:map(Fun,R),							%% 8337
	%%Fun = fun(K,V) -> K+1,V+1 end,		dict:map(Fun,R),						%% 12406
	%%Fun = fun(K,V) -> K+1,V+1 end,		gb_trees:map(Fun,R),						%% 25339
	ok;
test_struct(Num, R) ->
	NewR = [{Num, Num}|R], 	%%lists:keyfind(5000,1,NewR), 				%% 50736
	%%NewR = R#{Num=>Num}, 	 %%maps:get(5000, NewR, 0),					%% 4670
	%%NewR = dict:store(Num, Num, R), %%dict:find(5000, NewR),			%% 15750
	%%NewR = gb_trees:insert(Num,Num,R), %%gb_trees:lookup(5000, NewR),	%% 37047
	test_struct(Num-1, NewR).

%%timer:tc(lib_test, test_sort, [10000, lib_rand:random_list(lists:seq(1,10))]). 
test_sort(0, _) ->
	ok;
test_sort(Num, List) ->
	sort2(List),
	%%lists:sort(List),
	test_sort(Num-1, List).
%%%===================================================================
%%% Internal functions
%%%===================================================================

sort([]) ->
	[];
sort([X|T]) ->
	%%sort([ID||ID<-T, ID<X])++[X|sort([ID||ID<-T, ID>X])].
	split(T, X, [], []).

split([], X, L1, L2) ->
	sort(L1)++[X|sort(L2)];
split([H|T], X, L1, L2) when H<X ->
	split(T, X, [H|L1], L2);
split([H|T], X, L1, L2) ->
	split(T, X, L1, [H|L2]).


sort2([]) ->
	[];
sort2([X]) ->
	[X];
sort2([X,Y]) when X<Y ->
	[X,Y];
sort2([X,Y]) ->
	[Y,X];
sort2([X,Y|T]) when X<Y ->
	split2(X, Y, T, [], []);
sort2([X,Y|T]) ->
	split2(Y, X, T, [], []).

split2(X, Y, [], L1, L2) ->
	sort2(L1)++[X,Y|sort2(L2)];
split2(X, Y, [H|T], L1, L2) when H<X ->
	split2(X, Y, T, [H|L1], L2);
split2(X, Y, [H|T], L1, L2) when H<Y ->
	split2(X, H, T, L1, [Y|L2]);
split2(X, Y, [H|T], L1, L2) ->
	split2(X, Y, T, L1, [H|L2]).


% sort3([]) ->
% 	[];
% sort3([X]) ->
% 	[X];
% sort3([X,Y]) when X<Y ->
% 	[X,Y];
% sort3([X,Y]) ->
% 	[Y,X];
% sort3([X,Y|T]) when X<Y ->
% 	split_1(X, Y, T, [], []);
% sort3([X,Y|T]) ->
% 	split_1(Y, X, T, [], []).

% split_1(X, Y, [Z | L], R, Rs) when Z >= Y ->
%     split_1(Y, Z, L, [X | R], Rs);
% split_1(X, Y, [Z | L], R, Rs) when Z >= X ->
%     split_1(Z, Y, L, [X | R], Rs);
% split_1(X, Y, [Z | L], [], Rs) ->
%     split_1(X, Y, L, [Z], Rs);
% split_1(X, Y, [Z | L], R, Rs) ->
%     split_1_1(X, Y, L, R, Rs, Z);
% split_1(X, Y, [], R, Rs) ->
%     merge([[Y, X | R] | Rs]).

% split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= Y ->
%     split_1_1(Y, Z, L, [X | R], Rs, S);
% split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= X ->
%     split_1_1(Z, Y, L, [X | R], Rs, S);
% split_1_1(X, Y, [Z | L], R, Rs, S) when S =< Z ->
%     split_1(S, Z, L, [], [[Y, X | R] | Rs]);
% split_1_1(X, Y, [Z | L], R, Rs, S) ->
%     split_1(Z, S, L, [], [[Y, X | R] | Rs]);
% split_1_1(X, Y, [], R, Rs, S) ->
%     merge([[S], [Y, X | R] | Rs]).

% %% [[5,1],[10,7,6,2],[9,8,4,3]]
%%timer:tc(lib_test, merge, [[lists:reverse(lists:seq(1,ID))||ID<-lists:seq(1,1000)]]).  6174154
merge([List]) ->
	lists:reverse(List);
merge([L1, L2|T]) ->
	merge([merge2(L1, L2)|T]).

merge2([H1|T1], [H2|T2]) when H1=<H2 ->
	merge3(H1, H2, T1, T2, []);
merge2([H1|T1], [H2|T2]) ->
	merge3(H2, H1, T2, T1, []).

merge3(X, Y, L1, [], Acc) ->
	lists:reverse(Acc)++[Y,X|L1];
merge3(X, Y, L1, [H2|T2], Acc) when H2>=X ->
	merge3(X, H2, L1, T2, [Y|Acc]);
merge3(X, Y, L1, [H2|T2], Acc) ->
	merge3(H2, X, T2, L1, [Y|Acc]).

%%timer:tc(lib_test, rmergel, [[lists:reverse(lists:seq(1,ID))||ID<-lists:seq(1,1000)], []]).  102031
mergel([[] | L], Acc) ->
    mergel(L, Acc);
mergel([T1, [H2 | T2], [H3 | T3] | L], Acc) ->
    mergel(L, [merge3_1(T1, [], H2, T2, H3, T3) | Acc]);
mergel([T1, [H2 | T2]], Acc) ->
    rmergel([merge2_1(T1, H2, T2, []) | Acc], []);
mergel([L], []) ->
    L;
mergel([L], Acc) ->
    rmergel([lists:reverse(L, []) | Acc], []);
mergel([], []) ->
    [];
mergel([], Acc) ->
    rmergel(Acc, []);
mergel([A, [] | L], Acc) ->
    mergel([A | L], Acc);
mergel([A, B, [] | L], Acc) ->
    mergel([A, B | L], Acc).

rmergel([[H3 | T3], [H2 | T2], T1 | L], Acc) ->
    rmergel(L, [rmerge3_1(T1, [], H2, T2, H3, T3) | Acc]);
rmergel([[H2 | T2], T1], Acc) ->
    mergel([rmerge2_1(T1, H2, T2, []) | Acc], []);
rmergel([L], Acc) ->
    mergel([lists:reverse(L, []) | Acc], []);
rmergel([], Acc) ->
    mergel(Acc, []).

%% merge3/3

%% Take L1 apart.
merge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    merge2_1(T2, H3, T3, [H2 | M]);
merge3_1([], M, H2, T2, H3, T3) ->
    merge2_2(T2, H3, T3, M, H2).

%% Take L2 apart.
merge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    merge2_1(T1, H3, T3, [H1 | M]);
merge3_2(T1, H1, M, [], H3, T3) ->
    merge2_2(T1, H3, T3, M, H1).

% H1 =< H2. Inlined.
merge3_12(T1, H1, H2, T2, H3, T3, M) when H1 =< H3 ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12(T1, H1, H2, T2, H3, T3, M) ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3).

% H1 =< H2, take L3 apart.
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H1 =< H3 ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12_3(T1, H1, H2, T2, M, []) ->
    merge2_1(T1, H2, T2, [H1 | M]).

% H1 > H2. Inlined.
merge3_21(T1, H1, H2, T2, H3, T3, M) when H2 =< H3 ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21(T1, H1, H2, T2, H3, T3, M) ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3).

% H1 > H2, take L3 apart.
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H2 =< H3 ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_21_3(T1, H1, H2, T2, M, []) ->
    merge2_2(T1, H2, T2, M, H1).

%% rmerge/3

%% Take L1 apart.
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    rmerge2_2(T2, H3, T3, M, H2);
rmerge3_1([], M, H2, T2, H3, T3) ->
    rmerge2_1(T2, H3, T3, [H2 | M]).

%% Take L2 apart.
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    rmerge2_2(T1, H3, T3, M, H1);
rmerge3_2(T1, H1, M, [], H3, T3) ->
    rmerge2_1(T1, H3, T3, [H1 | M]).

% H1 =< H2. Inlined.
rmerge3_12(T1, H1, H2, T2, H3, T3, M) when H2 =< H3 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 =< H2, take L3 apart.
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H2 =< H3 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3);
rmerge3_12_3(T1, H1, H2, T2, M, []) ->
    rmerge2_2(T1, H2, T2, M, H1).

% H1 > H2. Inlined.
rmerge3_21(T1, H1, H2, T2, H3, T3, M) when H1 =< H3 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 > H2, take L3 apart.
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H1 =< H3 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3);
rmerge3_21_3(T1, H1, H2, T2, M, []) ->
    rmerge2_1(T1, H2, T2, [H1 | M]).

%% merge/2

merge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
    merge2_2(T1, H2, T2, M, H1);
merge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

merge2_2(T1, HdM, [H2 | T2], M, H1) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1, HdM | M]);
merge2_2(T1, HdM, [H2 | T2], M, H1) ->
    merge2_2(T1, H2, T2, [HdM | M], H1);
merge2_2(T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% rmerge/2

rmerge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    rmerge2_2(T1, H2, T2, M, H1);
rmerge2_1([H1 | T1], H2, T2, M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rmerge2_2(T1, HdM, [H2 | T2], M, H1) when H1 =< H2 ->
    rmerge2_2(T1, H2, T2, [HdM | M], H1);
rmerge2_2(T1, HdM, [H2 | T2], M, H1) ->
    rmerge2_1(T1, H2, T2, [H1, HdM | M]);
rmerge2_2(T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).