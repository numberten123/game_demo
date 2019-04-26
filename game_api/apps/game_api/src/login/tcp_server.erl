-module(tcp_server).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("tcp.hrl").
-include("all_pb.hrl").

%% API
-export([start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [[Ref, Socket, Transport, Opts]]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%					   {ok, State, Timeout} |
%%					   ignore |
%%					   {stop, Reason}
%% @end
%%--------------------------------------------------------------------
%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined};

init([Ref, Socket, Transport, _Opts]) ->
	{ok, {IP,_}} = Transport:peername(Socket),
	process_flag(trap_exit, true),
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
	Transport:setopts(Socket, [{active, ?TCP_ACTIVE}]),
	State = #tcp_state{
		ip = IP,
		socket = Socket,
		transport = Transport
	},
	erlang:send_after(?HEART_TIMEOUT, self(), heartbeat_timeout),	%%心跳包检测
	gen_server:enter_loop(?MODULE, [], State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%									 {reply, Reply, State} |
%%									 {reply, Reply, State, Timeout} |
%%									 {noreply, State} |
%%									 {noreply, State, Timeout} |
%%									 {stop, Reason, Reply, State} |
%%									 {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%									{noreply, State, Timeout} |
%%									{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%%转发协议到客户端
handle_cast({send_data, _Cmd, RawData}, State) ->
	#tcp_state{
		socket = Socket,
		transport = Transport
	} = State,
	catch Transport:send(Socket, RawData),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%									 {noreply, State, Timeout} |
%%									 {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Data}, State) ->
	#tcp_state{
		last_binary = LastBinary
	} = State,
	AddData = <<LastBinary/binary, Data/binary>>,	%%把tcp拆包合并
	NewState = decode_data(AddData, State),
	{noreply, NewState};

%%收到包到达设置
handle_info({tcp_passive, Socket}, State) ->
	#tcp_state{
	   socket = Socket,
	   transport = Transport,
	   last_passive_time = LastPsvTime
	} = State,
	TimeNow = lib_misc:now(milli_second),
	case TimeNow-LastPsvTime of
		R when R>?TCP_ACTIVE_TIME ->
			Transport:setopts(Socket, [{active, ?TCP_ACTIVE}]),
			NewState = State#tcp_state{
				last_passive_time = TimeNow
			},
			{noreply, NewState};
		_ ->%%在设置时间内，收到包过多	，断开连接
			{stop, {shutdown, tcp_passive_error}, State}
	end;

%%socket断开
handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

%%超时未收到心跳包
handle_info(heartbeat_timeout, State) ->
	#tcp_state{
		heart_beat_time = HBTime
	} = State,
	TimeNow = lib_misc:now(milli_second),
	case TimeNow-HBTime of
		R when R>=?HEART_TIMEOUT ->%%超时
			{stop, {shutdown, heartbeat_timeout}, State};
		_ ->
			erlang:send_after(?HEART_TIMEOUT, self(), heartbeat_timeout),
			{noreply, State}
	end;

handle_info(_Info, State) ->
	lager:error("_Info:~p~n", [_Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
	lager:error("terminate:~p~n", [Reason]),
	#tcp_state{
		socket = Socket,
		transport = Transport
	} = State,
	catch Transport:close(Socket),
	ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%处理协议，粘包拆包问题
decode_data(BinaryData, State) ->
	#tcp_state{
		is_login = IsLogin,
		role_pid = RolePid
	} = State,
	case BinaryData of
		<<DataSize:16, Cmd:16, RawData:DataSize/binary, Bin/binary>> ->
			State2 = case lib_misc:get_pb_cmd(Cmd) of
				10 when not IsLogin ->%%登录服
					case catch packet:unpack(Cmd, RawData) of
						{ok, Cmd, CmdData} ->%%处理协议
							{_, State1} = do_deal_cmd(Cmd, CmdData, State),
							State1;
						Error ->
							lager:error("unpack error:~p~n", [Error]),
							State
					end;
				_ when IsLogin ->%%游戏服
					catch gen_server:cast(RolePid, {recv_data, Cmd, RawData}),
					State;
				_ ->
					State
			end,
			case Bin of
				<<>> ->
					State2#tcp_state{last_binary = <<>>};
				_ ->
					decode_data(Bin, State2)
			end;
		_ ->
			State#tcp_state{last_binary = BinaryData}
			
	end.

do_deal_cmd(Cmd, DataIn, State) ->
	#tcp_state{
		socket = Socket,
		transport = Transport
	} = State,
	case catch login_cmd:handle(Cmd, DataIn, State) of
		{ok, Result} ->
			catch lib_misc:packet_and_send(Cmd, Result, Transport, Socket),
			{noreply, State};
		{ok, Result, NewState} ->
			catch lib_misc:packet_and_send(Cmd, Result, Transport, Socket),
			{noreply, NewState};
		{noreply, NewState} ->
			{noreply, NewState};
		noreply ->
			{noreply, State};
		{error, ErrorID} ->%%错误码
			Error = #m_1101_toc{cmd = Cmd, error_code = ErrorID},
			catch lib_misc:packet_and_send(1101, Error, Transport, Socket),
			{noreply, State};
		Error ->
			lager:error("login_cmd error:~p~n", [Error]),
			{noreply, State}
	end.