-module(data_upgrade).

-include("common.hrl").
-export([check_upgrade/0]).

%% 数据升级
check_upgrade() ->
	AllList = cfg_data_upgrade:get_all(),
	case catch lib_query:mysql_query(?POOL_GAME, "select id from data_upgrade where status=1") of
		{ok, ExecList} ->%%找出已执行的语句
			[begin
				case lists:member([ID], ExecList) of
					false ->%%未执行过
						do_upgrade(ID);
					_ ->
						ignore
				end
			end||ID<-AllList];
		_ ->%%数据表不存在,创建
			do_upgrade(1)
	end.

do_upgrade(ID) ->
	{Pool, SQL} = get_pool_and_sql(ID),
	NewSQL = unicode:characters_to_binary(SQL),
	case catch lib_query:mysql_query(Pool, NewSQL) of
		ok ->%%成功，更新表状态
			lager:info("do_upgrade id:~p~n",[ID]),
			lib_query:mysql_query(?POOL_GAME, "REPLACE INTO data_upgrade(id,status) VALUES (?,1);", [ID]);
		Error ->
			lager:error("do_upgrade error:~p~n", [{ID, Error}])
	end.

get_pool_and_sql(ID) ->
	case cfg_data_upgrade:get_single(ID) of
		#cfg_data_upgrade{upgrade_db = Pool, upgrade_params = SQL} ->
			{Pool, SQL};
		_ ->
			lager:error("check_upgrade error, not find id:~p~n", [ID]),
			{?POOL_GAME, "select * from data_upgrade"}
	end.