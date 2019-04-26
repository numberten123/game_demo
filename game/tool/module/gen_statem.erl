-module(gen_statem).
-behaviour(gen_statem).

-include("common.hrl").

%%桌子状态
-define(STATE_FREE, fish_free).         %%自由出鱼
-define(STATE_LANG, fish_lang).         %%刷浪
-define(STATE_GROUP, fish_group).       %%鱼阵

-export([start_link/1]).
-export([terminate/3,code_change/4,init/1,callback_mode/0,handle_event/4]).
-export([?STATE_FREE/2, ?STATE_LANG/2, ?STATE_GROUP/2]).

start_link([MgrPid, TableID, RoomIndex]) ->
    Name = mod_fish_mgr:get_name(?MODULE, TableID), %%进程名
    gen_statem:start_link({local, Name}, ?MODULE, [MgrPid, TableID, RoomIndex], []).

init([MgrPid, TableID, RoomIndex]) ->
    put(fish_id, 1),
    put(bullet_id, 1),
    #cfg_fish_table{
        rate = Rate,
        max_num = PerNum
    } = cfg_fish_table:get_single(TableID),
    Data = #fish_state{
        mgr_pid = MgrPid,
        rate = Rate,
        table_id = TableID,
        per_num = PerNum,
        room_index = RoomIndex,
        position = lists:seq(1,PerNum)
    },
    ets:insert(?ETS_ROOM_KV,#room_kv{room_id = RoomIndex#room_index.room_id,room_index = RoomIndex}),
    ets:insert(?ETS_ROOM,#room_data{id = RoomIndex,room_pid=self(),leader_id = 0,player_list = [],setting = []}),
    NewData = do_fish_free(Data),   %%出鱼期，随机出鱼
    erlang:send_after(?CLEAN_TIME, self(), clean_time_out_fish),
    erlang:send_after(?UPDATE_POOL_TIME, self(), update_fish_pool),
    {ok, ?STATE_FREE, NewData, ?STATE_FREE_TIME}.

callback_mode() -> handle_event_function.

%%只有状态timeout才改变状态
handle_event(timeout, _, StateName, Data) ->
    ?ERROR_MSG("timeout:~p~n",[StateName]),
    ?MODULE:StateName(timeout, Data);

%%处理其他消息
handle_event(Type, Msg, StateName, Data) ->
    case catch do_handle_event(Type, Msg, StateName, Data) of
        {stop, NewData} ->
            {stop, normal, NewData};
        {_,NewData} ->
            {next_state, StateName, NewData, get_remain_time(NewData)};
        Other ->
            ?ERROR_MSG("do_handle_event other:~p~n",[Other]),
            {next_state, StateName, Data, get_remain_time(Data)}
    end.

%% Mandatory callback functions
terminate(_Reason, _State, Data) ->
    ?ERROR_MSG("terminate:~p~n",[_Reason]),
    update_fish_pool(Data),
    #fish_state{ 
        room_index = RoomIndex,
        mgr_pid = MgrPid,
        role_info = RoleInfo,
        table_id = TableID
    } = Data,
    ets:delete(?FISH_TABLE, TableID),
    do_return_yuanbao(maps:keys(RoleInfo), Data),    %%桌子异常崩溃，退还元宝给玩家
    gen_server:cast(MgrPid, {table_down, RoomIndex, self()}),
    ets:delete(?ETS_ROOM,RoomIndex),
    ets:delete(?ETS_ROOM_KV,RoomIndex#room_index.room_id),
    void.

code_change(_Vsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%桌子状态切换(自由出鱼，刷浪，鱼阵)
?STATE_FREE(timeout, Data) ->
    NewData = do_fish_lang(Data),       %%刷浪，清鱼
    {next_state, ?STATE_LANG, NewData, ?STATE_LANG_TIME}.
?STATE_LANG(timeout, Data) ->
    NewData = do_fish_group(Data),      %%出鱼阵
    {next_state, ?STATE_GROUP, NewData, ?STATE_GROUP_TIME}.
?STATE_GROUP(timeout, Data) ->
    NewData = do_fish_free(Data),       %%出鱼期，随机出鱼
    {next_state, ?STATE_FREE, NewData, ?STATE_FREE_TIME}.

%%%===================================================================
%%% do_handle_event functions
%%%===================================================================
%%进桌
do_handle_event({call, From}, {enter_table, SelPositon, RoleItem}, _StateName, Data) ->
    #fish_state{
        role_info = RoleInfo,
        per_num = PerNum,
        player_id_list = PlayerIDList,
        rate = Rate,
        position = PositionList
    } = Data,
    #fish_table_role_info{
        player_id = PlayerID,
        yb_num = YbNum
    } = RoleItem,
    {Reply, NewData} = case length(PlayerIDList)>=PerNum of
        false ->
            case maps:find(PlayerID, RoleInfo) of
                error ->%%
                    case lists:member(SelPositon, PositionList) of
                        true ->%%座位可用
                            GetPosition = SelPositon,
                            NewPosition = lists:delete(SelPositon, PositionList);
                        _ ->
                            [GetPosition|NewPosition] = rand_list(PositionList)
                    end,
                    NewRoleItem = RoleItem#fish_table_role_info{
                        yb_num = 0,
                        yubi = YbNum*Rate,
                        position = GetPosition
                    },
                    Data1 = Data#fish_state{
                        player_id_list = [PlayerID|PlayerIDList],
                        role_info = RoleInfo#{PlayerID=>NewRoleItem},
                        position = NewPosition
                    },
                    update_table_ets(Data1), %%更新缓存
                    mod_fish_send:send_52002_enter(YbNum*Rate, PlayerID, Data1),
                    {ok, Data1};
                _ ->
                    {{error, ?E_FISH_IN_TABLE}, Data}
            end;
        _ ->
            {{error, ?E_FISH_TABLE_FULL}, Data}
    end,
    gen_statem:reply(From, Reply),
    mod_fish_send:send_52009_enter_leave(PlayerID, 1, NewData),     %%广播进桌
    {next_state, NewData};

%%协议处理
do_handle_event(cast, {cmd, Cmd, RecvData, PlayerID}, _StateName, Data) ->
    case catch mod_fish_cmd:handle({Cmd, RecvData, PlayerID}, Data) of
        {noreply, NewData} ->
            {next_state, NewData};
        {error, ErrorCode} ->
            pp_fish:send_error([PlayerID], Cmd, ErrorCode),
            {next_state, Data};
        {stop, NewData} ->
            {stop, NewData};
        Error ->
            ?ERROR_MSG("fish cmd error:~p~n",[Error]),
            {next_state, Data}
    end;

%%停服
do_handle_event(cast, server_down, _StateName, Data) ->
    {stop, Data};

%%出鱼
do_handle_event(info, {do_fish_free, TypeID}, ?STATE_FREE, Data) ->
    #fish_state{
        fish_info = FishInfo,
        ele_fish_id = EleFishID,
        ele_fish_rate = OldFishRate
    } = Data,
    #cfg_fish_type{
        time = Time,
        fish_id = FishID,
        fish_num = FishNum,
        track = Track
    } = cfg_fish_type:get_single(TypeID),
    erlang:send_after(Time*1000, self(), {do_fish_free, TypeID}),
    [OneFishTypeID|_] = rand_list(FishID),  %%随机一种鱼
    [OneTrackID|_] = rand_list(Track),  %%随机一条路线
    FishUnionID = get_fish_union_id(FishNum, []),         %%
    MinFishID = lists:min(FishUnionID), %%鱼群开始id
    #cfg_fish_info{
        spec_id = SpecID
    } = cfg_fish_info:get_single(OneFishTypeID),
    FishData = #s2c_52004_fish{
        track_id = OneTrackID,
        num = FishNum,
        fish_id = MinFishID,
        fish_type_id = OneFishTypeID,
        time = util:unixtime()
    },
    case SpecID of
        1 ->%%电鱼
            DoSpawEleFish = fun(EleRate) ->
                FishData2 = FishData#s2c_52004_fish{rate = EleRate},
                NewFishInfo = add_new_fish_info(FishInfo, FishData2, FishUnionID),   %%保存新鱼信息
                NewData = Data#fish_state{
                    fish_info = NewFishInfo,
                    ele_fish_id = MinFishID,
                    ele_fish_rate = EleRate,
                    last_shock_time = 0
                },
                mod_fish_send:send_52004_fish(FishData2, Data),     %%推送出鱼
                {next_state, NewData}
            end,
            CanCreate = case maps:find(EleFishID, FishInfo) of
                {ok, FishItem} ->%%之前的电鱼还在
                    maps:size(clean_time_out_fish(#{EleFishID=>FishItem})) == 0;    %%过期也可生成新
                _ ->
                    true
            end,
            case EleFishID of
                0 when CanCreate ->%%被打死，初始化倍率
                    DoSpawEleFish(50);
                _ when CanCreate ->%%继承上次倍率
                    DoSpawEleFish(OldFishRate);
                _ ->%%不生成电鱼
                    {next_state, Data}
            end;
        _ ->%%其他鱼
            NewFishInfo = add_new_fish_info(FishInfo, FishData, FishUnionID),   %%保存新鱼信息
            NewData = Data#fish_state{
                fish_info = NewFishInfo
            },
            mod_fish_send:send_52004_fish(FishData, Data),     %%推送出鱼
            {next_state, NewData}
    end;

%%清理过期鱼
do_handle_event(info, clean_time_out_fish, _StateName, Data) ->
    erlang:send_after(?CLEAN_TIME, self(), clean_time_out_fish),
    #fish_state{
        fish_info = FishInfo
    } = Data,
    NewData = Data#fish_state{
        fish_info = clean_time_out_fish(FishInfo)
    },
    {next_state, NewData};

%%同步彩池
do_handle_event(info, update_fish_pool, _StateName, Data) ->
    erlang:send_after(?UPDATE_POOL_TIME, self(), update_fish_pool),
    NewData = update_fish_pool(Data),
    {next_state, NewData};

do_handle_event(Type, Msg, _StateName, Data) ->
    ?ERROR_MSG("do_handle_event other:~p~n",[{Type, Msg}]),
    {next_state, Data}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%随机出鱼，清空鱼阵
do_fish_free(Data) ->
    AllList = cfg_fish_type:get_all(),
    [begin
        #cfg_fish_type{
            time = Time
        } = cfg_fish_type:get_single(ID),
        erlang:send_after(Time*1000, self(), {do_fish_free, ID})
    end||ID<-AllList],
    Data#fish_state{
        fish_info=#{},
        next_state_time = get_next_time(?STATE_FREE_TIME)
    }.

%%清空鱼,切场景
do_fish_lang(Data) ->
    #fish_state{
        scene = Scene
    } = Data,
    mod_fish_send:send_52008_wave(Data),     %%通知刷浪
    Data#fish_state{
        fish_info=#{},
        scene = Scene rem 2+1,
        next_state_time = get_next_time(?STATE_LANG_TIME)
    }.

%%出鱼阵
do_fish_group(Data) ->
    Data#fish_state{
        next_state_time = get_next_time(?STATE_GROUP_TIME)
    }.

do_return_yuanbao(LeaveList, Data) ->
    #fish_state{
        role_info = RoleInfo,
        rate = Rate
    } = Data,
    Fun = fun(PlayerID) ->
        #fish_table_role_info{
            yubi = YuBi
        } = maps:get(PlayerID, RoleInfo),
        AddYuanBao = YuBi div Rate,
        lib_player_data:add_player_diamond(PlayerID, AddYuanBao, ?GAME_ID_FISH, ?SOURCE_FISH_LEAVE),
        ets:update_element(?ETS_ROOM_PLAYER_DATA,PlayerID,{#player_data.room_id,0}),
        mod_fish_send:send_52003_leave(AddYuanBao, PlayerID)
    end,
    lists:foreach(Fun, LeaveList).

get_next_time(Time) ->
    util:now_ms()+Time.
%%剩余时间
get_remain_time(Data) ->
    #fish_state{
        next_state_time = NextStateTime
    } = Data,
    case NextStateTime-util:now_ms() of
        R when R>0 ->
            R;
        _ ->
            0
    end.

rand_list(List) ->
    [X|| {_,X} <- lists:sort([{rand:uniform(), N} || N <- List])].

get_fish_union_id(FishNum, List) when FishNum=<0 ->
    List;
get_fish_union_id(FishNum, List) ->
    get_fish_union_id(FishNum-1, [do_get(fish_id)|List]).

%%唯一id
do_get(K) ->
    V = get(K),
    case V>=500 of
        true ->%%1-500循环
            put(K, 1);
        _ ->
            put(K, V+1)
    end.

clean_time_out_fish(FishInfo) ->
    Fun = fun(K, V, Acc) ->
        #s2c_52004_fish{
            track_id = TrackID,
            time = Time
        } = V,
        #cfg_fish_track{
            time = AliveTime
        } = cfg_fish_track:get_single(TrackID),
        case util:unixtime()>=Time+AliveTime of
            true ->%%过期鱼
                Acc;
            _ ->
                Acc#{K=>V}
        end
    end,
    maps:fold(Fun, #{}, FishInfo).
    
add_new_fish_info(FishInfo, _FishData, []) ->
    FishInfo;
add_new_fish_info(FishInfo, FishData, [H|T]) ->
    add_new_fish_info(FishInfo#{H=>FishData#s2c_52004_fish{fish_id=H}}, FishData, T).

%%同步彩池
update_fish_pool(Data) ->
    #fish_state{
        add_pool = AddPool,
        mgr_pid = MgrPid
    } = Data,
    gen_server:cast(MgrPid, {update_fish_pool, AddPool}),
    Data#fish_state{
        add_pool=#{}
    }.

update_table_ets(Data) ->
    #fish_state{
        table_id = TableID
    } = Data,
    RoleInfo = mod_fish_send:get_send_role_info(Data),
    Item = #ets_fish_table{
        table_id = TableID,
        info = RoleInfo
    },
    ets:insert(?FISH_TABLE, Item).