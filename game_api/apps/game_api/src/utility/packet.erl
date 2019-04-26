-module(packet).

%% API TO PACK PB
-export([pack/2,
		 unpack/2,
		 pack_pb/1,
		 unpack_pb/2]
		 ).

%%%===================================================================
%%% API
%%%===================================================================
pack(Cmd, Data) ->
	Bin = all_pb:encode_msg(Data),
	DataSize = erlang:byte_size(Bin),
	RawBin	 = <<DataSize:16, Cmd:16, Bin/binary>>,
	{ok, RawBin}.

unpack(Cmd, Bin) ->
	String = "m_" ++ erlang:integer_to_list(Cmd) ++ "_tos",
	Name = lib_misc:list_to_atom(String),
	try all_pb:decode_msg(Bin, Name) of
		Data ->
			{ok, Cmd, Data}
	catch
		Class:Reason ->
			lager:error("~n error in unpack, Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
			error
	end.

pack_pb(Data) ->
	all_pb:encode_msg(Data).

unpack_pb(Name, Bin)->
	try all_pb:decode_msg(Bin, Name) of
		Data ->
			Data
	catch
		Class:Reason ->
			lager:error("~n error in unpack_pb, Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
			undefined
	end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
