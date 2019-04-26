-module(web_server).

-define(OK, <<"ok">>).

-export([init/2]).

init(Req, State) ->
    What = cowboy_req:binding(what, Req),
    Bin = handle(What, Req),
    Result = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, Bin, Req),
    {ok, Result, State}.

handle(<<"test">>, _Req) ->
	<<"test">>;
handle(_What, _Req) ->
	?OK.