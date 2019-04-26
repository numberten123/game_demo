-module(transform).

-export([
    %% 序列化和反序列化
    term_to_string/1
    ,term_to_binary/1
    ,term_to_binary2/1
    ,binary_to_term/1
    ,string_to_term/1
    ,string_to_term/2
    %% 基本转化
    ,to_atom/1
    ,to_binary/1
    ,to_string/1
    ,to_list/1
    ,to_integer/1
    ,encode/1
]).

%% -> list()
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~w", [Term]))).

%% -> binary()
term_to_binary(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% ~w格式化
%% -> binary()
term_to_binary2(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
%% -> undefined | term()
string_to_term(String) when is_list(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
					Term;
                _Err ->
					undefined
			end;    
        _Error ->
            undefined
    end;
string_to_term(String) when is_binary(String) ->
    string_to_term(binary_to_list(String));
string_to_term(Other) -> 
    Other.

string_to_term(String, Def) -> 
    case string_to_term(String) of
        undefined -> Def;
        Term -> Term
    end.

binary_to_term(Binary) ->
    string_to_term(Binary).



%% 转换为atom
to_atom(Val) when is_atom(Val) -> Val;
to_atom(Val) when is_list(Val) -> lib_misc:list_to_atom(Val);
to_atom(Val) when is_binary(Val) -> to_atom(binary_to_list(Val));
to_atom(Val) -> Val.

%% 转换为integer
to_integer(null) -> 0;
to_integer(undefined) -> 0;
to_integer([]) -> 0;
to_integer(Val) when is_integer(Val) -> Val;
to_integer(Val) when is_binary(Val) -> to_integer(binary_to_list(Val));
to_integer(Val) when is_list(Val) -> to_integer(list_to_integer(Val));
to_integer(Val) when is_float(Val) -> to_integer(float_to_list(Val));
to_integer(Val) -> Val.

%% 转换为list
to_string(Val) when is_list(Val) -> Val;
to_string(Val) when is_binary(Val) -> binary_to_list(Val);
to_string(Val) when is_integer(Val) -> integer_to_list(Val);
to_string(Val) when is_atom(Val) -> atom_to_list(Val);
to_string(Val) when is_pid(Val) -> pid_to_list(Val);
to_string(Val) when is_tuple(Val) -> term_to_string(Val);
to_string(Val) -> Val.

%% same as to_string/1
to_list(Val) -> to_string(Val).

%% 转换为binary
to_binary(Val) when is_integer(Val) -> list_to_binary(integer_to_list(Val));
to_binary(Val) when is_float(Val) -> list_to_binary(float_to_list(Val));
to_binary(Val) when is_list(Val) -> list_to_binary(Val);
to_binary(Val) when is_tuple(Val) -> transform:term_to_binary(Val);
to_binary(Val) when is_binary(Val) -> Val;
to_binary(Val) when is_atom(Val) -> list_to_binary(to_string(Val));
to_binary(_Val) -> <<>>.

encode(List) -> 
  Bin = list_to_binary([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(List))]),
  binary_to_list(Bin).