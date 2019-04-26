%%%-------------------------------------------------------------------
%%% @author mac
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 九月 2018 10:58
%%%-------------------------------------------------------------------
-module(lib_rsa).
-author("mac").

-include_lib("public_key/include/public_key.hrl").



%% API
-export([
    set_private_key/0,
    en_rsa/1,
    de_rsa/1,
    key_to_list/1,
    cipher_to_list/1,
    list_to_cipher/1,
    to_hex/1
]).




% RSA

set_private_key() ->
    %%获取私钥
    #'RSAPrivateKey'{modulus=Modulus, publicExponent=PublicExponent} = PrivateKey = public_key:generate_key({rsa, 1024, 65537}),
    %%获取公钥
    PublicKey = #'RSAPublicKey'{modulus=Modulus, publicExponent=PublicExponent},

    put(rsa_private_key, PrivateKey),
    put(rsa_public_key, PublicKey),
    {PrivateKey, PublicKey}.

en_rsa(BinData) ->
    %%公钥加密
    PublicKey = get(rsa_public_key),
    Cipher = public_key:encrypt_public(BinData, PublicKey),
    Cipher.


de_rsa(BinData) ->
    PrivateKey = get(rsa_private_key),
    %%私钥解密
    Result = public_key:decrypt_private(BinData, PrivateKey),
    Result.

%%公钥转字符串
key_to_list(PublicKey) ->
    binary_to_list(public_key:pem_encode([public_key:pem_entry_encode('SubjectPublicKeyInfo',PublicKey)])).

%%密文转16进制字符
cipher_to_list(Cipher) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(Cipher)]).

%%16进制字符转二进制密文
list_to_cipher(List) ->
    list_to_cipher(List, []).

list_to_cipher([], Result) ->
    list_to_binary(lists:reverse(Result));
list_to_cipher([A,B|T], Result) ->
    Int = list_to_integer([A,B],16),
    list_to_cipher(T, [Int|Result]).

%%转16进制
to_hex(Num) ->
    to_hex(Num, []).

to_hex(0, Acc) ->
    Acc;
to_hex(I, Acc) ->
    to_hex(I bsr 4, [hexdigit(I band 15) | Acc]).

hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.