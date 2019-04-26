-module(web_socket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).


init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, State};

websocket_handle({binary, Data}, State) ->
    {reply, {binary, Data}, State};

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.