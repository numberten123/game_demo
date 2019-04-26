-module(lib_misc).

-include("common.hrl").
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS,86400).

%% 这个模块放置erlang的通用方法
%% API
-compile(export_all).
%%%===================================================================
%%% API
%%%===================================================================
encode_md5(List) ->
	list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(List))]).

list_to_atom(List) ->
	case catch list_to_existing_atom(List) of
		{'EXIT', _} ->
			erlang:list_to_atom(List);
		Atom ->
			Atom
	end.

%% 取得当前unix时间戳，精确到秒
now() ->
	erlang:system_time(seconds).

%% 取得当前unix时间戳，精确到微秒
now(micro) ->
	erlang:system_time(micro_seconds);

%% 取得当前unix时间戳，精确到毫秒
now(milli_second) ->
	erlang:system_time(milli_seconds).

packet_and_send(Cmd, Result, Transport, Socket) ->
	case Transport of
		undefined ->
			ok;
		_ ->
			try packet:pack(Cmd, Result) of
				{ok, RawBin} ->
					Transport:send(Socket, RawBin)
			catch
		        Class:Reason ->
		            lager:error("Cmd:~p, Result:~p, packet_and_send error:~s", [Cmd, Result, lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})])
		    end
	end.

send_dataout(SocketPid, Cmd, Data) ->
	case SocketPid of
		undefined ->
			ok;
		_ ->
			try packet:pack(Cmd, Data) of
				{ok, RawBin} ->
					gen_server:cast(SocketPid, {send_data, Cmd, RawBin}) 
			catch
		        Class:Reason ->
		        	lager:error("Cmd~p, Result~p~n",[Cmd, Data]),
		            lager:error("~n send_dataout error:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})])
		    end
	end.

is_remote_process_alive(Pid) ->
	case catch rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
		true ->
			true;
		_ ->
			false
	end.

is_same_date(Seconds1, Seconds2) ->
	{{Year1, Month1, Day1}, _Time1} = seconds_to_localtime(Seconds1),
	{{Year2, Month2, Day2}, _Time2} = seconds_to_localtime(Seconds2),
	if ((Year1 /= Year2) or (Month1 /= Month2) or (Day1 /= Day2)) -> false;
	   true -> true
	end.

is_same_week(NowTime, EndTime) ->
	Week = get_week_date(EndTime),
	Time1 = EndTime - Week * 86400,
	{Date1, _} = lib_misc:seconds_to_localtime(Time1), 					%%这周开始的日期
	WeekBeginTime = mktime({Date1, {23,59,59}}), 				%%这周开始的日期的最后一秒
	Time2 = EndTime + (7-Week) * 86400,
	{Date2, _} = seconds_to_localtime(Time2), 					%%这周最后一天的日期
	WeekEndTime = mktime({Date2, {23,59,59}}), 				%%这周最后一天的日期的最后一秒
	case NowTime > WeekBeginTime of
		true when NowTime =< WeekEndTime ->
			true;
		_ ->
			false
	end.

seconds_to_localtime(Seconds) ->
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1900),
	calendar:universal_time_to_local_time(DateTime).

%%获取今天的24:00:00 和今天的）00:00:00的秒数
get_midnight_seconds(Seconds) ->
	{{_Year, _Month, _Day}, Time} = seconds_to_localtime(Seconds),
	% 从午夜到现在的秒数
	Diff   = calendar:time_to_seconds(Time),
	% 获取当天0点
	Today  = Seconds - Diff,
	% 获取第二天0点
	NextDay = Seconds + (?ONE_DAY_SECONDS-Diff),
	{Today, NextDay}.

get_localtime() ->
	Seconds = lib_misc:now(),
	DateTime = calendar:gregorian_seconds_to_datetime(Seconds+?DIFF_SECONDS_0000_1900),
	calendar:universal_time_to_local_time(DateTime).

get_localtime_string() ->
	%% DATETIME
	%% 一个日期和时间组合。支持的范围是'1000-01-01 00:00:00'到'9999-12-31 23:59:59'。MySQL以'YYYY-MM-DD HH:MM:SS'格式来显示DATETIME值，
	%% 但是允许你使用字符串或数字把值赋给DATETIME的列。
	{{Year, Month, Day}, {Hour, Minute, Second}} = lib_misc:get_localtime(),
	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",[Year, Month, Day, Hour, Minute, Second])).

mktime({Date, Time}) ->
	%% 	DATETIME
	%% 一个日期和时间组合。支持的范围是'1000-01-01 00:00:00'到'9999-12-31 23:59:59'。MySQL以'YYYY-MM-DD HH:MM:SS'格式来显示DATETIME值，
	%% 但是允许你使用字符串或数字把值赋给DATETIME的列。
	DT = erlang:localtime_to_universaltime({Date, Time}),
	calendar:datetime_to_gregorian_seconds(DT) - ?DIFF_SECONDS_0000_1900.

%% 获取周几
get_week_date(Seconds) ->
	{Date, _} = seconds_to_localtime(Seconds),
	calendar:day_of_the_week(Date).

%% 现在的分钟数
get_now_min() ->
	{H, M, _} = erlang:time(),
	H*60+M.

%% 现在的秒数
get_now_sec() ->
	{H, M, S} = erlang:time(),
	H*3600+M*60+S.

%% 2天是否连续
%% Day2 必须要大于 Day1
is_two_day_consecutive(Day1, Day2) ->
	Seconds1 = calendar:datetime_to_gregorian_seconds({Day1, {12, 0, 0}}),
	Seconds2 = calendar:datetime_to_gregorian_seconds({Day2, {12, 0, 0}}),
	Seconds2  == (Seconds1 + 86400).

do_binary_to_term(Bin) ->
	case catch binary_to_term(Bin, [safe]) of
		{'EXIT', _Reason} ->
			catch binary_to_term(Bin);
		Result ->
			Result
	end.

%%获取协议模块号
get_pb_cmd(Cmd) when Cmd>10000 ->
	Cmd div 1000;
get_pb_cmd(Cmd) ->
	Cmd div 100.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_post_url_result() ->
	PostList = [{"player_id", 100005},{"value",100}],
	catch httpc:request(post, {"http://10.168.100.52:9096/send_to_agent", [], "application/json", term_to_binary(mochijson2:encode(PostList))}, [{timeout, 5000}], [{body_format,binary}]).