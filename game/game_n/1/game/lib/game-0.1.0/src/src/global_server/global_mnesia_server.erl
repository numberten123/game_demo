-module(global_mnesia_server).

%%do mnesia dirty query

-include("common.hrl").

-behaviour(gen_server).
%% API
-export([start_link/0
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-export([
	set_mnesia_value/1,
	update_mnesia_value/2,
	call_mnesia_value/2
]).

-record(state, {
	}).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%%脏写插入
set_mnesia_value(Value) ->
	gen_server:cast({global, ?MODULE}, {set_mnesia_value, Value}).

%%先读后写
update_mnesia_value(Table, Args) ->
	gen_server:cast({global, ?MODULE}, {update_mnesia_value, Table, Args}).

%%同步操作mnesia
call_mnesia_value(Table, Args) ->
	gen_server:call({global, ?MODULE}, {call_mnesia_value, Table, Args}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({call_mnesia_value, Table, Args}, _From, State) ->
	Reply = do_call_mnesia(Table, Args),
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({set_mnesia_value, Value}, State) ->
	mnesia:dirty_write(Value),
	{noreply, State};

handle_cast({update_mnesia_value, Table, Args}, State) ->
	do_update_mnesia(Table, Args),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_call_mnesia(?DATA_MNESIA_KV, {get_value, Key}) ->
	lib_query:get_mnesia_value(?DATA_MNESIA_KV, Key);
do_call_mnesia(_Table, _Args) ->
	ok.

%%增加节点信息
do_update_mnesia(Table, {add_node_info, Node, ServerIP, GamePort}) ->
	{ok, ModeType} = application:get_env(game, node_mode),
	NodeItem = #node_info{
		node = Node,
	   	ip = ServerIP,
	   	port = GamePort,
	   	num = 0
	},
	AddItem = case lib_query:get_mnesia_value(Table, ModeType) of
		R = #mnesia_node_info{nodes = Nodes} ->
			R#mnesia_node_info{
				nodes = Nodes#{Node=>NodeItem} 
			};
		_ ->
			#mnesia_node_info{
				type = ModeType,
				nodes = #{Node=>NodeItem} 
			}
	end,
	mnesia:dirty_write(AddItem);

%%删除节点信息
do_update_mnesia(Table, {del_node_info, Node}) ->
	{ok, ModeType} = application:get_env(game, node_mode),
	case lib_query:get_mnesia_value(Table, ModeType) of
		R = #mnesia_node_info{nodes = Nodes} ->
			NewItem = R#mnesia_node_info{
				nodes = maps:remove(Node, Nodes)
			},
			mnesia:dirty_write(NewItem);
		_ ->
			ignore
	end;

do_update_mnesia(_Table, _Args) ->
	ok.