-record(tcp_state, {
	is_login = false,	%% 是否登录成功
	socket,
	transport,
	ip,
	role_pid,				%%玩家进程
	last_binary = <<>>,		%%解决tcp拆包问题
	last_passive_time=0,	%%tcp上次passive时间
	heart_beat_time=0		%%上次收到心跳包时间
}).

%%监听类型
-define(TCP_TYPE_GAME, tcp_game_server).

%% 游戏服的auth_key超时时间，单位是毫秒
-define(AUTHKEY_TIMEOUT, 300000).

%%心跳包超时时间,前端5秒发一次
-define(HEART_TIMEOUT, 60000).

%% gen_tcp active参数
-define(TCP_ACTIVE, 300).

%% gen_tcp active超时
-define(TCP_ACTIVE_TIME, 10000).