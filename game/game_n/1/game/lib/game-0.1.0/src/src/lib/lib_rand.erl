%%----------------------------------------------------

%% @doc 随机数种子服务器
%%
%% @end
%%----------------------------------------------------
-module(lib_rand).

-export([get_seed/0,
		 random_member/1,
		 range/2,
		 range/1,
		 test/1,
		 split/2,
		 split/1,
		 take_by_prop/2,
		 take_by_weight/2,
		 take_by_weights_list/1,
		 random_list/1]).

%% 尽量不要使用这个方法，只是为了兼容旧方法
get_seed() ->
	erlang:timestamp().

%% 产生一个介于Min到Max之间的随机整数
range(Same, Same) ->
	Same;
range(Min, Max) ->
	M = Min - 1,
	rand:uniform(abs(Max - M)) + M.

%% 从一个list中随机取出一项
random_member(List) ->
	RandomId = rand:uniform(length(List)),
	lists:nth(RandomId, List).

%% 从一个list中随机取出一项
%% null | term()
%% 废弃方法，尽量使用random_member/1方法
range([]) -> null;
range([I]) -> I;
range(List) ->
	[H|_] = random_list(List),
	H.

%% 概率命中
%% @return true | false
test(Rate) ->
	R = round(Rate * 100),
	range(1, 10000) =< R.

%% 从一个list中随机取出N项,并返回这N项和剩余的项
%% @return {TakenList, RestList}
split(N, List) ->
	TakenList = take_by_prop(List, N),
	RestList  = List -- TakenList,
	{TakenList, RestList}.

%% same as take(List, 1)
%% 从一个list中随机取出一项,并返回这个项,和剩余的项
%% @return {Term, RestList} | null
split([]) -> null;
split(List) ->
	[H|Tail] = random_list(List),
	{H, Tail}.

%% @doc 从[{Item, Proportion}...]列表中按照概率抽取不重复的多个元组
%% @spec [{Item, Proportion}...], int() -> [{Item, Proportion}...]
take_by_prop(List, N) ->
	RandomList = random_list(List),
	lists:sublist(RandomList, N).

%% 按照概率从列表中取出项， List是[{Item, ItemProb}...]形式, Prob和ItemProb是相同的单位数值
take_by_weight(Prob, List) ->
	take_by_weight(Prob, 0, List).

%% 权重获取列表中的序列
%% List::[{ID, Weights}]
%% ID::integer(), Weights::integer()
take_by_weights_list(List) ->
	{ID, _} = range(List),
	ID.

%% 把一个列表给随机排列一下元素
random_list(List) ->
	[X||{_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

take_by_weight(Prob, Acc, [{Item, ItemProb} | Tail]) ->
	case Tail of
		[] ->
			Item;
		_ ->
			NewAcc = Acc + ItemProb,
			case NewAcc >= Prob of
				true ->
					Item;
				false ->
					take_by_weight(Prob, NewAcc, Tail)
			end
	end.