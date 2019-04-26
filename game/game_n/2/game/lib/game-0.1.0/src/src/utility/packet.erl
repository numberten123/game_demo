-module(packet).

%% API
-export([pack/2,
		 unpack/2,
		 pack_pb/2,
		 unpack_pb/2]
		 ).

%%%===================================================================
%%% API
%%%===================================================================
pack(Cmd, Data) ->
	String	 = "encode_m_" ++ erlang:integer_to_list(Cmd) ++ "_toc",
	Fun		 = lib_misc:list_to_atom(String),
	PbList	 = all_pb:Fun(Data),
	Bin		 = erlang:iolist_to_binary(PbList),
	DataSize = erlang:byte_size(Bin),
	RawBin	 = <<DataSize:16, Cmd:16, Bin/binary>>,
	{ok, RawBin}.

unpack(Cmd, Bin) ->
	String = "decode_m_" ++ erlang:integer_to_list(Cmd) ++ "_tos",
	Fun	   = lib_misc:list_to_atom(String),
	%% 在版本升级时，服务器的版本会落后客户端
	try all_pb:Fun(Bin) of
		Data ->
			{ok, Cmd, Data}
	catch
		Class:Reason ->
			lager:error("~n error in unpack, Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
			error
	end.

pack_pb(Pb, Data) ->
	String	 = "encode_" ++ Pb,
	Fun		 = lib_misc:list_to_atom(String),
	PbList	 = all_pb:Fun(Data),
	erlang:iolist_to_binary(PbList).

unpack_pb(Pb, Bin)->
	String = "decode_" ++ Pb,
	Fun	   = lib_misc:list_to_atom(String),
	try all_pb:Fun(Bin) of
		Data ->
			Data
	catch
		Class:Reason ->
			lager:error("~n error in unpack_pb, Stacktrace:~s", [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]),
			undefine
	end.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
