-module(login_cmd).

-include("common.hrl").
-include("tcp.hrl").

%% API
-export([handle/3]).

%%%===================================================================
%%% API
%%%===================================================================
%%心跳包
handle(1001, _DataIn, State) ->
	TimeNow = lib_misc:now(),
	Result = #m_1001_toc{
		time = TimeNow
	},
	NewState = State#tcp_state{
		heart_beat_time = TimeNow
	},
	{ok, Result, NewState};

%%
handle(1002, _DataIn, _State) ->
	{_, PublicKey} = lib_rsa:set_private_key(),
	BinKey = lib_rsa:key_to_bin(PublicKey),
	Result = #m_1002_toc{
		public_key = BinKey
	},
	{ok, Result};

handle(1003, DataIn, _State) ->
	#m_1003_tos{
		username = UserName,
		password = Password
	} = DataIn,
	case check_user_info(UserName, Password) of
		{ok, RoleID} ->%%检测帐号密码

			{ok, RoleID};
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
			case lib_rsa:de_rsa(transform:to_binary(Password)) of
				SetPwd ->%%密码对的上
					{ok,1};
				_ ->
					{error, ?ERMSG_PWD_NO_MATCH}
			end;
		_ ->
			{error, ?ERMSG_ACCOUNT_NOT_FIND}
	end.