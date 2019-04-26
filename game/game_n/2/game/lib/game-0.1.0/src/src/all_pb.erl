-file("src/all_pb.erl", 1).

-module(all_pb).

-export([encode_m_1090_toc/1, decode_m_1090_toc/1,
	 delimited_decode_m_1090_toc/1, encode_m_1090_tos/1,
	 decode_m_1090_tos/1, delimited_decode_m_1090_tos/1,
	 encode_m_1003_toc/1, decode_m_1003_toc/1,
	 delimited_decode_m_1003_toc/1, encode_m_1003_tos/1,
	 decode_m_1003_tos/1, delimited_decode_m_1003_tos/1,
	 encode_m_1002_toc/1, decode_m_1002_toc/1,
	 delimited_decode_m_1002_toc/1, encode_m_1002_tos/1,
	 decode_m_1002_tos/1, delimited_decode_m_1002_tos/1,
	 encode_m_1001_toc/1, decode_m_1001_toc/1,
	 delimited_decode_m_1001_toc/1, encode_m_1001_tos/1,
	 decode_m_1001_tos/1, delimited_decode_m_1001_tos/1,
	 encode_m_1101_toc/1, decode_m_1101_toc/1,
	 delimited_decode_m_1101_toc/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2, delimited_decode/2]).

-export([int_to_enum/2, enum_to_int/2]).

-record(m_1090_toc, {}).

-record(m_1090_tos, {role_id, auth_key}).

-record(m_1003_toc, {ip, port, role_id, auth_key}).

-record(m_1003_tos, {username, password}).

-record(m_1002_toc, {public_key}).

-record(m_1002_tos, {}).

-record(m_1001_toc, {time}).

-record(m_1001_tos, {}).

-record(m_1101_toc, {cmd, error_code, description}).

encode([]) -> [];
encode(Records) when is_list(Records) ->
    delimited_encode(Records);
encode(Record) -> encode(element(1, Record), Record).

encode_m_1090_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1090_toc(Record)
    when is_record(Record, m_1090_toc) ->
    encode(m_1090_toc, Record).

encode_m_1090_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1090_tos(Record)
    when is_record(Record, m_1090_tos) ->
    encode(m_1090_tos, Record).

encode_m_1003_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1003_toc(Record)
    when is_record(Record, m_1003_toc) ->
    encode(m_1003_toc, Record).

encode_m_1003_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1003_tos(Record)
    when is_record(Record, m_1003_tos) ->
    encode(m_1003_tos, Record).

encode_m_1002_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1002_toc(Record)
    when is_record(Record, m_1002_toc) ->
    encode(m_1002_toc, Record).

encode_m_1002_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1002_tos(Record)
    when is_record(Record, m_1002_tos) ->
    encode(m_1002_tos, Record).

encode_m_1001_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1001_toc(Record)
    when is_record(Record, m_1001_toc) ->
    encode(m_1001_toc, Record).

encode_m_1001_tos(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1001_tos(Record)
    when is_record(Record, m_1001_tos) ->
    encode(m_1001_tos, Record).

encode_m_1101_toc(Records) when is_list(Records) ->
    delimited_encode(Records);
encode_m_1101_toc(Record)
    when is_record(Record, m_1101_toc) ->
    encode(m_1101_toc, Record).

encode(m_1101_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1101_toc, Record) ->
    [iolist(m_1101_toc, Record)
     | encode_extensions(Record)];
encode(m_1001_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1001_tos, Record) ->
    [iolist(m_1001_tos, Record)
     | encode_extensions(Record)];
encode(m_1001_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1001_toc, Record) ->
    [iolist(m_1001_toc, Record)
     | encode_extensions(Record)];
encode(m_1002_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1002_tos, Record) ->
    [iolist(m_1002_tos, Record)
     | encode_extensions(Record)];
encode(m_1002_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1002_toc, Record) ->
    [iolist(m_1002_toc, Record)
     | encode_extensions(Record)];
encode(m_1003_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1003_tos, Record) ->
    [iolist(m_1003_tos, Record)
     | encode_extensions(Record)];
encode(m_1003_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1003_toc, Record) ->
    [iolist(m_1003_toc, Record)
     | encode_extensions(Record)];
encode(m_1090_tos, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1090_tos, Record) ->
    [iolist(m_1090_tos, Record)
     | encode_extensions(Record)];
encode(m_1090_toc, Records) when is_list(Records) ->
    delimited_encode(Records);
encode(m_1090_toc, Record) ->
    [iolist(m_1090_toc, Record)
     | encode_extensions(Record)].

encode_extensions(_) -> [].

delimited_encode(Records) ->
    lists:map(fun (Record) ->
		      IoRec = encode(Record),
		      Size = iolist_size(IoRec),
		      [protobuffs:encode_varint(Size), IoRec]
	      end,
	      Records).

iolist(m_1101_toc, Record) ->
    [pack(1, required,
	  with_default(Record#m_1101_toc.cmd, none), uint32, []),
     pack(2, required,
	  with_default(Record#m_1101_toc.error_code, none),
	  uint32, []),
     pack(3, optional,
	  with_default(Record#m_1101_toc.description, none),
	  string, [])];
iolist(m_1001_tos, _Record) -> [];
iolist(m_1001_toc, Record) ->
    [pack(1, required,
	  with_default(Record#m_1001_toc.time, none), uint32,
	  [])];
iolist(m_1002_tos, _Record) -> [];
iolist(m_1002_toc, Record) ->
    [pack(1, required,
	  with_default(Record#m_1002_toc.public_key, none), bytes,
	  [])];
iolist(m_1003_tos, Record) ->
    [pack(1, required,
	  with_default(Record#m_1003_tos.username, none), string,
	  []),
     pack(2, required,
	  with_default(Record#m_1003_tos.password, none), string,
	  [])];
iolist(m_1003_toc, Record) ->
    [pack(1, required,
	  with_default(Record#m_1003_toc.ip, none), string, []),
     pack(2, required,
	  with_default(Record#m_1003_toc.port, none), string, []),
     pack(3, required,
	  with_default(Record#m_1003_toc.role_id, none), uint32,
	  []),
     pack(4, required,
	  with_default(Record#m_1003_toc.auth_key, none), bytes,
	  [])];
iolist(m_1090_tos, Record) ->
    [pack(1, required,
	  with_default(Record#m_1090_tos.role_id, none), uint32,
	  []),
     pack(2, required,
	  with_default(Record#m_1090_tos.auth_key, none), bytes,
	  [])];
iolist(m_1090_toc, _Record) -> [].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(pikachu, value) -> 1.

int_to_enum(_, Val) -> Val.

decode_m_1090_toc(Bytes) when is_binary(Bytes) ->
    decode(m_1090_toc, Bytes).

decode_m_1090_tos(Bytes) when is_binary(Bytes) ->
    decode(m_1090_tos, Bytes).

decode_m_1003_toc(Bytes) when is_binary(Bytes) ->
    decode(m_1003_toc, Bytes).

decode_m_1003_tos(Bytes) when is_binary(Bytes) ->
    decode(m_1003_tos, Bytes).

decode_m_1002_toc(Bytes) when is_binary(Bytes) ->
    decode(m_1002_toc, Bytes).

decode_m_1002_tos(Bytes) when is_binary(Bytes) ->
    decode(m_1002_tos, Bytes).

decode_m_1001_toc(Bytes) when is_binary(Bytes) ->
    decode(m_1001_toc, Bytes).

decode_m_1001_tos(Bytes) when is_binary(Bytes) ->
    decode(m_1001_tos, Bytes).

decode_m_1101_toc(Bytes) when is_binary(Bytes) ->
    decode(m_1101_toc, Bytes).

delimited_decode_m_1101_toc(Bytes) ->
    delimited_decode(m_1101_toc, Bytes).

delimited_decode_m_1001_tos(Bytes) ->
    delimited_decode(m_1001_tos, Bytes).

delimited_decode_m_1001_toc(Bytes) ->
    delimited_decode(m_1001_toc, Bytes).

delimited_decode_m_1002_tos(Bytes) ->
    delimited_decode(m_1002_tos, Bytes).

delimited_decode_m_1002_toc(Bytes) ->
    delimited_decode(m_1002_toc, Bytes).

delimited_decode_m_1003_tos(Bytes) ->
    delimited_decode(m_1003_tos, Bytes).

delimited_decode_m_1003_toc(Bytes) ->
    delimited_decode(m_1003_toc, Bytes).

delimited_decode_m_1090_tos(Bytes) ->
    delimited_decode(m_1090_tos, Bytes).

delimited_decode_m_1090_toc(Bytes) ->
    delimited_decode(m_1090_toc, Bytes).

delimited_decode(Type, Bytes) when is_binary(Bytes) ->
    delimited_decode(Type, Bytes, []).

delimited_decode(_Type, <<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
delimited_decode(Type, Bytes, Acc) ->
    try protobuffs:decode_varint(Bytes) of
      {Size, Rest} when size(Rest) < Size ->
	  {lists:reverse(Acc), Bytes};
      {Size, Rest} ->
	  <<MessageBytes:Size/binary, Rest2/binary>> = Rest,
	  Message = decode(Type, MessageBytes),
	  delimited_decode(Type, Rest2, [Message | Acc])
    catch
      _What:_Why -> {lists:reverse(Acc), Bytes}
    end.

decode(enummsg_values, 1) -> value1;
decode(m_1101_toc, Bytes) when is_binary(Bytes) ->
    Types = [{3, description, string, []},
	     {2, error_code, uint32, []}, {1, cmd, uint32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1101_toc, Decoded);
decode(m_1001_tos, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1001_tos, Decoded);
decode(m_1001_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, time, uint32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1001_toc, Decoded);
decode(m_1002_tos, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1002_tos, Decoded);
decode(m_1002_toc, Bytes) when is_binary(Bytes) ->
    Types = [{1, public_key, bytes, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1002_toc, Decoded);
decode(m_1003_tos, Bytes) when is_binary(Bytes) ->
    Types = [{2, password, string, []},
	     {1, username, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1003_tos, Decoded);
decode(m_1003_toc, Bytes) when is_binary(Bytes) ->
    Types = [{4, auth_key, bytes, []},
	     {3, role_id, uint32, []}, {2, port, string, []},
	     {1, ip, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1003_toc, Decoded);
decode(m_1090_tos, Bytes) when is_binary(Bytes) ->
    Types = [{2, auth_key, bytes, []},
	     {1, role_id, uint32, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1090_tos, Decoded);
decode(m_1090_toc, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(m_1090_toc, Decoded).

decode(<<>>, Types, Acc) ->
    reverse_repeated_fields(Acc, Types);
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
      {FNum, Name, Type, Opts} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1) | List]}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keyfind('$extensions', 2, Acc) of
	    {_, _, Dict} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

reverse_repeated_fields(FieldList, Types) ->
    [begin
       case lists:keyfind(FNum, 1, Types) of
	 {FNum, Name, _Type, Opts} ->
	     case lists:member(repeated, Opts) of
	       true -> {FNum, Name, lists:reverse(Value)};
	       _ -> Field
	     end;
	 _ -> Field
       end
     end
     || {FNum, Name, Value} = Field <- FieldList].

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(m_1101_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1101_toc),
						   Record, Name, Val)
			  end,
			  #m_1101_toc{}, DecodedTuples),
    Record1;
to_record(m_1001_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1001_tos),
						   Record, Name, Val)
			  end,
			  #m_1001_tos{}, DecodedTuples),
    Record1;
to_record(m_1001_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1001_toc),
						   Record, Name, Val)
			  end,
			  #m_1001_toc{}, DecodedTuples),
    Record1;
to_record(m_1002_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1002_tos),
						   Record, Name, Val)
			  end,
			  #m_1002_tos{}, DecodedTuples),
    Record1;
to_record(m_1002_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1002_toc),
						   Record, Name, Val)
			  end,
			  #m_1002_toc{}, DecodedTuples),
    Record1;
to_record(m_1003_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1003_tos),
						   Record, Name, Val)
			  end,
			  #m_1003_tos{}, DecodedTuples),
    Record1;
to_record(m_1003_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1003_toc),
						   Record, Name, Val)
			  end,
			  #m_1003_toc{}, DecodedTuples),
    Record1;
to_record(m_1090_tos, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1090_tos),
						   Record, Name, Val)
			  end,
			  #m_1090_tos{}, DecodedTuples),
    Record1;
to_record(m_1090_toc, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       m_1090_toc),
						   Record, Name, Val)
			  end,
			  #m_1090_toc{}, DecodedTuples),
    Record1.

decode_extensions(Record) -> Record.

decode_extensions(_Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{FNum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keyfind(FNum, 1, Types) of
	       {FNum, Name, Type, Opts} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal = decode(Type, V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{FNum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{FNum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> -1.

extension_size(_) -> 0.

has_extension(_Record, _FieldName) -> false.

get_extension(_Record, _FieldName) -> undefined.

set_extension(Record, _, _) -> {error, Record}.

