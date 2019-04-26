-module(lib_hu).

%%麻将牌花色
-define(PAI_WAN, 1).    %%万，1-9
-define(PAI_TIAO, 2).   %%条，1-9
-define(PAI_TONG, 3).   %%筒，1-9
-define(PAI_FENG, 4).   %%风，1-7

-export([check_hu/1, check_lai_hu/2]).

%%牌结构，#{花色=>牌列表}， 例：#{1=>[1,2,3,3,4,5],2=>[1,2,3],3=>[1,1]}
%%坎牌：123    刻牌：111
%%判断胡牌思路：去掉一对将牌，剩下的牌分花色判断是否由坎牌和刻牌组成即可
%%判断癞子胡牌逻辑：手牌去癞子，根据牌型分析获得癞子的组合牌型，和去癞子牌merge后再判断胡即可,
%%判断癞子例子：lib_hu:check_lai_hu(#{1=>[8],3=>[1,1,2,2,6,7,9,9]}, 5).
%%特殊牌型，七对，十三幺，将一色，风一色等，(判断较为简单）

%%检查胡，不包含特殊牌型
check_hu(PaiMaps) ->
    MapsList = move_dui_pai_list(PaiMaps),      %%去掉一对将
    Fun = fun(OneMaps, false) ->
            Fun1 = fun(K, V, true) ->
                    check_be_kan_ke(K, sort_pai(V));      %%检查该花色是否都是坎刻
                (_,_,Acc) ->
                    Acc
            end,
            maps:fold(Fun1, true, OneMaps);
        (_,Acc) ->
            Acc
    end,
    lists:foldl(Fun, false, MapsList).

%%判断癞子胡牌
check_lai_hu(PaiMaps, LaiZiNum) ->
    CombinList = get_lai_combin_list(LaiZiNum, PaiMaps),    %%获取癞子组合
    %%merge癞子组合与去癞子的牌型，然后去检查胡
    check_combin_hu(PaiMaps, CombinList).

%%合并癞子组合与原牌型
check_combin_hu(_PaiMaps, []) ->
    false;
check_combin_hu(PaiMaps, [H|T]) ->
    Fun = fun({K, AddV}, Acc) ->
        case maps:find(K, Acc) of
            {ok, V} ->
                maps:put(K, lists:merge(AddV, V), Acc);
            _ ->
                maps:put(K, AddV, Acc)
        end
    end,
    NewPaiMaps = lists:foldl(Fun, PaiMaps, H),	%%和癞子组合形成的最终手牌
    case check_hu(NewPaiMaps) of
        false ->%%判断能胡，即癞子胡牌
            check_combin_hu(PaiMaps, T);
        _ ->
            true
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%去掉可能的将牌，形成多种牌型列表
move_dui_pai_list(PaiMaps) ->
    Fun = fun(K, V, Acc) ->
        case length(V) of
            L when L>1 ->
                SortV = sort_pai(V),
                Num = L - 2 +1, %%判断次数
                Fun02 = fun(Begin, Acc2) ->
                    Pai1 = lists:nth(Begin, SortV),
                    Pai2 = lists:nth(Begin+1, SortV),
                    case Pai1 of
                        Pai2 ->%%一样
                            NewV1 = lists:delete(Pai1, V),
                            NewV2 = lists:delete(Pai2, NewV1),
                            NewPaiMaps = case NewV2 of
                                [] ->
                                    maps:remove(K, PaiMaps);
                                _ ->
                                    PaiMaps#{K=>NewV2}
                            end,
                            case lists:member(NewPaiMaps, Acc2) of
                                true ->%%已存在
                                    Acc2;
                                _ ->
                                    [NewPaiMaps|Acc2]
                            end;
                        _ ->
                            Acc2
                    end
                end,
                lists:foldl(Fun02, Acc, lists:seq(1,Num));
            _ ->
                Acc
        end
    end,
    maps:fold(Fun, [], PaiMaps).

%%风牌只能为刻牌
check_be_kan_ke(?PAI_FENG, List) ->
    check_is_ke(List);
check_be_kan_ke(_, List) ->
    ComList = get_kan_ke(List),   %%分析刻牌，坎牌的组合
    lists:member(List, ComList).  %%原先牌型属于一种组合牌型即可

%%检查是否刻牌
check_is_ke([]) ->
    true;
check_is_ke([A,A,A|T]) ->
    check_is_ke(T);
check_is_ke(_) ->
    false.

%%分析该花色牌型，得到刻牌，坎牌的组合
get_kan_ke(List) ->
    case get_all_kan_ke(sort_pai(List)) of
        [] ->   
            [];
        AllKanKe ->%%筛选组合
            Fun = fun(OneKanKe, Acc) ->
                case check_pai_is_member(List, OneKanKe) of
                    true ->
                        NewList = do_del_list(List, OneKanKe),
                        case get_kan_ke(NewList) of
                            [] ->
                                [OneKanKe|Acc];
                            List2 ->%%和剩余牌坎坷组合
                                Item = [lists:merge(OneKanKe, L2)||L2<-List2],
                                lists:merge(Item, Acc)
                        end;
                    _ ->
                        Acc
                end
            end,
            lists:usort(lists:foldl(Fun, [], AllKanKe))
    end.

do_del_list(List, []) ->
    List;
do_del_list([], _) ->
    [];
do_del_list(List, [H|T]) ->
    do_del_list(lists:delete(H, List), T).


%%获取所有可能坎刻组合
get_all_kan_ke(List) ->
    Fun = fun(OnePai, Acc) ->
        Kan = [OnePai,OnePai+1,OnePai+2],
        Ke = [OnePai,OnePai,OnePai],
        NewAcc = case check_pai_is_member(List, Kan) of
            true ->
                [Kan|Acc];
            _ ->
                Acc
        end,
        case check_pai_is_member(List, Ke) of
            true ->
                [Ke|NewAcc];
            _ ->
                NewAcc
        end
    end,
    lists:usort(lists:foldl(Fun, [], List)).

%%判断B是否属于A
check_pai_is_member(_List, []) ->
    true;
check_pai_is_member(List, [H|T]) ->
    case lists:member(H, List) of
        true ->
            check_pai_is_member(lists:delete(H, List), T);  %%检查一次后删掉一张
        _ ->
            false
    end.

%%从小到大排序
sort_pai(List) ->
    lists:sort(fun(A, B) -> A<B end, List).

%%平胡癞子组合，特殊胡牌额外添加组合
get_lai_combin_list(LaiZiNum, PaiMaps) ->
    case maps:size(PaiMaps) of
        0 ->%%手上全是癞子
            case LaiZiNum of
                8 ->
                    [[{K,[2,2,5,5,5,8,8,8]}]||K<-lists:seq(1,?PAI_FENG)];
                _ ->
                    [[{K,lists:duplicate(LaiZiNum, 2)}]||K<-lists:seq(1,?PAI_FENG)]
            end;
        _ ->
            DuiList = get_lai_dui(LaiZiNum, PaiMaps),       %%找出形成一对的可能牌型
            Fun = fun({Num, List, Maps}, Acc) ->
                case Num of
                    0 ->%%没有癞子
                        lists:merge([List], Acc);
                    _ ->
                        MergeRke = case do_to_be_ke(Num, Maps) of
                            [] ->%%癞子做刻牌
                                [];
                            Rke ->
                                merge_list([List], Rke)
                        end,
                        MergeRkan = case do_to_be_kan(Num, Maps) of
                            [] ->%%癞子做坎牌
                                [];
                            Rkan ->
                                merge_list([List], Rkan)
                        end,
                        Acc1 = lists:merge(MergeRke, Acc),
                        lists:merge(MergeRkan, Acc1)
                end
            end,
            ResultList = lists:foldl(Fun, [], DuiList),
            %%todo 额外添加特殊胡牌组合
            ResultList
    end.

get_lai_dui(LaiZiNum, PaiMaps) ->
    Fun0 = fun(K, V, Acc) ->
        case length(V) of
            L when L>1 ->
                SortV = sort_pai(V),
                Num = L - 2 +1, %%判断次数
                Fun02 = fun(Begin, Acc2) ->
                    Pai1 = lists:nth(Begin, SortV),
                    Pai2 = lists:nth(Begin+1, SortV),
                    case Pai1 of
                        Pai2 ->%%一样
                            NewV1 = lists:delete(Pai1, V),
                            NewV2 = lists:delete(Pai2, NewV1),
                            NewPaiMaps = case NewV2 of
                                [] ->
                                    maps:remove(K, PaiMaps);
                                _ ->
                                    PaiMaps#{K=>NewV2}
                            end,
                            Item = {LaiZiNum, [], NewPaiMaps},
                            case lists:member(Item, Acc2) of
                                true ->%%已存在
                                    Acc2;
                                _ ->
                                    [Item|Acc2]
                            end;
                        _ ->
                            Acc2
                    end
                end,
                lists:foldl(Fun02, Acc, lists:seq(1,Num));
            _ ->
                Acc
        end
    end,
    Dui0 = maps:fold(Fun0, [], PaiMaps),
    Fun1 = fun(K, V, Acc) ->
        Fun12 = fun(OnePai, Acc2) ->
            NewV = lists:delete(OnePai, V),
            NewPaiMaps = case NewV of
                [] ->
                    maps:remove(K, PaiMaps);
                _ ->
                    PaiMaps#{K=>NewV}
            end,
            Item = {LaiZiNum-1, [{K,[OnePai]}], NewPaiMaps},
            case lists:member(Item, Acc2) orelse lists:member(OnePai, NewV) of
                true ->%%已存在,或者本来存在
                    Acc2;
                _ ->
                    [Item|Acc2]
            end
        end,
        lists:foldl(Fun12, Acc, V)
    end,
    Dui1 = maps:fold(Fun1, Dui0, PaiMaps),
    case LaiZiNum>1 of
        true ->
            [OneKey|_] = maps:keys(PaiMaps),
            Fun2 = fun(K, Acc) ->
                Item = {LaiZiNum-2, [{K,[2,2]}], PaiMaps},
                [Item|Acc]
            end,
            lists:foldl(Fun2, Dui1, [OneKey]);
        _ ->
            Dui1
    end.

merge_list(List, []) ->
    List;
merge_list([], List) ->
    List;
merge_list(List1, List2) ->
    [lists:merge(A,B)||A<-List1, B<-List2].

do_to_be_ke(LaiZiNum, PaiMaps) ->
    case maps:size(PaiMaps) of
        0 ->
            get_to_be_ke(LaiZiNum);
        _ ->
            Fun = fun(K, V, {Num, L}) ->
                NewV = del_kan(K,V),
                Fun1 =fun(OnePai, {Num2, L2, InL}) ->
                    HasCheck = lists:member(OnePai, InL),
                    SameNum = get_same_num(OnePai, NewV, 0),    %%重复牌数量
                    case SameNum rem 3 of
                        0 ->
                            {Num2, L2, InL};
                        _ when HasCheck ->%%已经检查
                            {Num2, L2, InL};
                        Rem ->
                            NNum = 3-Rem,
                            NList = lists:duplicate(NNum, OnePai),
                            {Num2+NNum, [{K,NList}|L2], [OnePai|InL]}
                    end
                end,
                {NewNum, NewL, _} = lists:foldl(Fun1, {Num, L, []}, NewV),
                {NewNum, NewL}
            end,
            {NeedNum, List} = maps:fold(Fun, {0, []}, PaiMaps),
            case LaiZiNum - NeedNum of
                R when R >=0 ->
                    case R rem 3 of
                        0 ->
                            merge_list([List], get_to_be_ke(R));
                        _ ->
                            []
                    end;
                _ ->
                    []
            end
    end.

get_to_be_ke(0) ->
    [];
get_to_be_ke(LaiZiNum) ->
    KeList = case LaiZiNum of
        3 ->
            [2,2,2];
        6 ->
            [2,2,2,5,5,5];
        _ ->
            []
    end,
    Fun = fun(K, Acc) ->
        [[{K, KeList}]|Acc]
    end,
    lists:foldl(Fun, [], lists:seq(1,?PAI_FENG)).

get_same_num(_OnePai, [], Num) ->
    Num;
get_same_num(OnePai, [OnePai|T], Num) ->
    get_same_num(OnePai, T, Num+1);
get_same_num(OnePai, [_|T], Num) ->
    get_same_num(OnePai, T, Num).

del_kan(?PAI_FENG,List) ->
    List;
del_kan(_,List) ->
    Fun = fun(OnePai, Acc) ->
        {Del1, Del2} = case OnePai of
            1 ->
                {2,3};
            9 ->
                {7,8};
            _ ->
                {OnePai-1,OnePai+1}
        end,
        case lists:member(Del1, Acc) andalso lists:member(Del2, Acc) andalso lists:member(OnePai, Acc) of
            true ->
                lists:delete(OnePai,lists:delete(Del2,lists:delete(Del1, Acc)));
            _ ->
                Acc
        end
    end,
    lists:foldl(Fun, List, List).

do_to_be_kan(LaiZiNum, PaiMaps) ->
    Fun = fun(K, V, Acc) ->
        case get_kan_ke(V) of
            [] ->%%不用删
                DealList = [[{K,[OnePai]}||OnePai<-ItemList]||ItemList<-do_get_may_be(sort_pai(V))],
                merge_list(DealList, Acc);
            DelList ->
                Fun1 = fun(DelItem, Acc1) ->
                    List = do_del_list(V, DelItem), %%执行删除策略
                    DealList = [[{K,[OnePai]}||OnePai<-ItemList]||ItemList<-do_get_may_be(sort_pai(List))],
                    case DealList of
                        [] ->
                            [[]|Acc1];
                        _ ->
                            lists:merge(DealList, Acc1)
                    end
                end,
                AllDealList = lists:foldl(Fun1, [], DelList),
                merge_list(AllDealList, Acc)
        end
    end,
    MaybeList = maps:fold(Fun, [], PaiMaps),
    [List||List<-MaybeList, length(List)==LaiZiNum].

do_get_may_be([]) ->
    [];
do_get_may_be([9]) ->
    [[7,8],[9,9]];
do_get_may_be([8]) ->
    [[7,9],[6,7],[8,8]];
do_get_may_be([1]) ->
    [[2,3],[1,1]];
do_get_may_be([2]) ->
    [[1,3],[3,4],[2,2]];
do_get_may_be([Num]) ->
    [[Num-1,Num+1],[Num-2,Num-1],[Num+1,Num+2],[Num,Num]];
do_get_may_be([1,2]) ->
    [[3]];
do_get_may_be([8,9]) ->
    [[7]];
do_get_may_be([Num1,Num2]) when Num2-Num1==1 ->
    [[Num1-1],[Num2+1]];
do_get_may_be(List) ->
    Len = length(List),
    Fun = fun(Key, {Num, Acc}) ->
        case Key of
            Num ->
                case Key of
                    Len ->%%最后一个
                        LastOne = lists:last(List),
                        {Num, merge_list(Acc, do_get_may_be([LastOne]))};
                    _ ->
                        Pai1 = lists:nth(Key, List),
                        Pai2 = lists:nth(Key+1, List),
                        case Pai2-Pai1 of
                            0 ->
                                {Num+2, merge_list(Acc, [[Pai1]])};
                            1 ->
                                {Num+2, merge_list(Acc, do_get_may_be([Pai1,Pai2]))};
                            2 ->
                                {Num+2, merge_list(Acc, [[Pai1+1]])};
                            _ ->
                                {Num+1, merge_list(Acc, do_get_may_be([Pai1]))}
                        end
                end;
            _ ->
                {Num, Acc}
        end
    end,
    {_, Result} = lists:foldl(Fun, {1, []}, lists:seq(1,Len)),
    Result.