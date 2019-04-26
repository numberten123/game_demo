%%%-------------------------------------------------------------------
%% @doc game_api public API
%% @end
%%%-------------------------------------------------------------------

-module(game_api_app).

-include("common.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case game_api_sup:start_link() of
    	{ok, Pid} ->
    		init_tcp_server(),		%%初始化tcp线程池
    		{ok, Pid};
    	{error, _Reason} ->
			lager:error("start error, _Reason:~p~n", [_Reason]),
			exit(1)
	end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
%%初始化tcp
init_tcp_server() ->
	{ok, GamePort} = application:get_env(game_api, game_port),
	{ok, _} = ranch:start_listener(?TCP_TYPE_GAME, 50, ranch_tcp, [{port, GamePort}, {max_connections, infinity}, {keepalive, true}], tcp_server, [{type, ?TCP_TYPE_GAME}]),
	ok.