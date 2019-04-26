-module(lib_rsa).

-include_lib("public_key/include/public_key.hrl").

%% API
-export([
    set_private_key/0,
    en_rsa/1,
    de_rsa/1,
    key_to_bin/1
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

%%公钥转二进制
key_to_bin(PublicKey) ->
	public_key:pem_encode([public_key:pem_entry_encode('SubjectPublicKeyInfo',PublicKey)]).