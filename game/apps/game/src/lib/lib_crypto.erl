%%%-------------------------------------------------------------------
%%% 本模块使用des ecb 方法加密,解密
%%%
%%%-------------------------------------------------------------------
-module(lib_crypto).

%% API
-export([rand_keys/0,
		 encrypt/2,
		 decrypt/2
		]).

%%%===================================================================
%%% API
%%%===================================================================
rand_keys() ->
	crypto:strong_rand_bytes(8).

%% PassWord 加密使用的密钥
%% BinText 要加密的二进制数据
encrypt(PassWord, BinText) when byte_size(PassWord) == 8, is_binary(BinText) ->
	encrypt_chunk(PassWord, BinText).

%% PassWord 加密使用的密钥
%% BinText 解密的二进制数据
decrypt(PassWord, BinText) when byte_size(PassWord) == 8, is_binary(BinText) ->
	binary_to_list(decrypt_chunk(PassWord, BinText)).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 使用0填充方式补全。
%% 刚好8位文本的情况下，得出的结果跟标准库输出结果不会相同，因为标准库不使用0填充.
encrypt_chunk(PassWord, <<Chunk:8/binary, Rest/binary>>) ->
	<<(crypto:block_encrypt(des_ecb, PassWord, Chunk))/binary, (encrypt_chunk(PassWord, Rest))/binary>>;

encrypt_chunk(PassWord, <<>>) ->
	crypto:block_encrypt(des_ecb, PassWord, <<0:64>>);

encrypt_chunk(PassWord, BinTex) ->
	PadSize = (8 - byte_size(BinTex)) * 8,
	PadBin = <<BinTex/binary, 0:PadSize>>,
	crypto:block_encrypt(des_ecb, PassWord, PadBin).

decrypt_chunk(PassWord, <<Chunk:8/binary, Rest/binary>>) ->
	case Rest of
		<<>> ->
			Result = crypto:block_decrypt(des_ecb, PassWord, Chunk),
			delete_pading(Result);
		_ ->
			<<(crypto:block_decrypt(des_ecb, PassWord, Chunk))/binary ,(decrypt_chunk(PassWord, Rest))/binary>>
	end.
%% des总是8位的
%%decrypt_chunk(PassWord, BinTex) ->
%%	Result = crypto:block_decrypt(des_ecb, PassWord, BinTex),
%%	delete_pading(Result).

delete_pading(<<0:64>>) ->
	<<>>;
delete_pading(<<A, 0:56>>) ->
	<<A>>;
delete_pading(<<A, B, 0:48>>) ->
	<<A, B>>;
delete_pading(<<A, B, C, 0:40>>) ->
	<<A, B, C>>;
delete_pading(<<A, B, C, D, 0:32>>) ->
	<<A, B, C, D>>;
delete_pading(<<A, B, C, D, E, 0:24>>) ->
	<<A, B, C, D, E>>;
delete_pading(<<A, B, C, D, E, F, 0:16>>) ->
	<<A, B, C, D, E, F>>;
delete_pading(<<A, B, C, D, E, F, G, 0:8>>) ->
	<<A, B, C, D, E, F, G>>;
delete_pading(Binary) ->
	Binary.
