-module(login_cmd).

-include("common.hrl").

%% API
-export([handle/3]).

%%%===================================================================
%%% API
%%%===================================================================
%%心跳包
handle(1001, _DataIn, State) ->
	TimeNow = lib_misc:now(milli_second),
	Result = #m_1001_toc{
		time = TimeNow div 1000
	},
	NewState = State#tcp_state{
		heart_beat_time = TimeNow
	},
	{ok, Result, NewState};

%%获取公钥
handle(1002, _DataIn, _State) ->
	{_, PublicKey} = lib_rsa:set_private_key(),
	#'RSAPublicKey'{modulus=Modulus, publicExponent=PublicExponent} = PublicKey,
	Result = #m_1002_toc{
		n = lib_rsa:to_hex(Modulus),
        e = lib_rsa:to_hex(PublicExponent)
	},
	{ok, Result};

%%玩家登录
handle(1003, DataIn, State) ->
	#m_1003_tos{
		username = UserName,
		password = Password
	} = DataIn,
	GetPwd = lib_rsa:de_rsa(transform:to_binary(Password)),
	case check_user_info(UserName, GetPwd) of
		{ok, RoleID} ->%%检测帐号密码
			{ok, GameNode} = application:get_env(game_api, game_node),
			{ok, RolePid} = rpc:call(GameNode, role_server, init_role, [RoleID]),
			erlang:link(RolePid),
			NewState = State#tcp_state{
				is_login = true,
				role_pid = RolePid
			},
			{ok, #m_1003_toc{}, NewState};
		Error ->
			Error
	end;

handle(_Cmd, _DataIn, _State) ->
	lager:error("login_cmd error:~p~n", [_Cmd]),
	noreply.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_user_info(UserName, Password) ->
	SQL = "select id, password from account where username=?",
	case lib_query:mysql_query(?POOL_GAME, SQL, [UserName]) of
		{ok, [[_ID, SetPwd]]} ->
			case Password of
				SetPwd ->%%密码对的上
					{ok,1};
				_ ->
					{error, ?ERMSG_PWD_NO_MATCH}
			end;
		_ ->
			{error, ?ERMSG_ACCOUNT_NOT_FIND}
	end.