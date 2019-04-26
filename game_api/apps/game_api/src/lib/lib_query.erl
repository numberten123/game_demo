-module(lib_query).

-compile(export_all).
-include("common.hrl").

%%%===================================================================
%%% API
%%%===================================================================
mysql_query(Pool, SQL) ->
	case catch mysql_poolboy:query(Pool, SQL) of
		ok ->
			ok;
		{ok,_,Result} ->
			{ok, Result};
		Error ->
            lager:error("mysql_query/2 Error:~p, Pool:~p, SQL:~p~n", [Error, Pool, SQL]),
			error
	end.

mysql_query(Pool, SQL, Para) ->
	case mysql_check_para(Para) of
		true ->
			case catch mysql_poolboy:query(Pool, SQL, Para) of
				ok ->
					ok;
				{ok,_,Result} ->
					{ok, Result};
				Error ->
		            lager:error("mysql_query/3 Error:~p, Pool:~p, SQL:~p, Para:~p~n", [Error, Pool, SQL, Para]),
					error
			end;
		_ ->
			lager:error("mysql_query/3 Para error:~p~n", [{SQL, Para}]),
			error
	end.

transaction_query(Pid, SQL, Para) ->
	case mysql_check_para(Para) of
		true ->
		    case catch mysql:query(Pid, SQL, Para) of
		        ok ->
		            ok;
		        {ok,_,Result} ->
		            {ok, Result};
		        Error ->
		        	lager:error("transaction_query/3 Error:~p~n", [Error]),
		            error
		    end;
		_ ->
			lager:error("transaction_query/3 Para error:~p~n", [{SQL, Para}]),
			error
	end.

get_redis_key(Table, RoleID) ->
	Table++":"++integer_to_list(RoleID).

set_redis_value(Key, Value) ->
	eredis_pool:q(pool1, ["SET", Key, Value]).

get_redis_value(Key) ->
	case eredis_pool:q(pool1, ["GET", Key]) of
		{ok, undefined} ->
			[];
		{ok, BinResult} ->
			BinResult
	end.
del_redis_value(Key) ->
	eredis_pool:q(pool1, ["DEL", Key]).

%%redis持久化
save_redis_to_disk() ->
	eredis_pool:q(pool1, ["BGSAVE"]).

%%mongo插入[#{<<"key">> => <<"Yankees">>]
mongodb_insert(TableName, Data) ->
	mongo_api:insert(mongodb_pool, TableName, Data).

%%mongo读取 #{<<"key">> => <<"123">>}
mongodb_find(TableName, Selector, Projector) ->
	mongo_api:find_one(mongodb_pool, TableName, Selector, Projector).

%%mneisa操作
get_mnesia_value(Table, Key) ->
	case mnesia:dirty_read(Table, Key) of
		[Item|_] ->
			Item;
		_ ->
			[]
	end.

set_mnesia_value(Value) ->
	global_mnesia_server:set_mnesia_value(Value).

update_mnesia_value(Table, Args) ->
	global_mnesia_server:update_mnesia_value(Table, Args).

call_mnesia_value(Table, Args) ->
	global_mnesia_server:call_mnesia_value(Table, Args).
%%%===================================================================
%%% Internal functions
%%%===================================================================
mysql_check_para(Para) ->
	case erlang:is_list(Para) of
		true ->
			case lists:member(undefined, Para) of
				true ->
					error;
				_ ->
					true
			end;
		_ ->
			error
	end.