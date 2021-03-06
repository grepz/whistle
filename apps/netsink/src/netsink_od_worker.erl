%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 17 May 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(netsink_od_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([set_types/2, data_process/3]).

-define(SERVER, ?MODULE).

-define(STATUS_INIT, worker_init).

-include_lib("whistle_misc/include/logging.hrl").
-include_lib("netsink/include/netsink.hrl").

-record(s, {
          watcher = undefined,
          idx = undefined,
          types = [],
          unprocessed = [],
          templates = []}
       ).

-define(worker_set_types(Types), {set_types, Types}).
-define(worker_data_process(Header, Data), {worker_data_process, Header, Data}).

%%%===================================================================
%%% API
%%%===================================================================

-spec set_types(Pid :: pid(), Types :: list:list(term())) -> ok | {error, term()}.
set_types(Pid, Types) ->
    gen_server:call(Pid, ?worker_set_types(Types)).

-spec data_process(Pid :: pid(), Header :: binary(), Data :: binary()) -> ok.
data_process(Pid, Header, Data) ->
    ok = gen_server:cast(Pid, ?worker_data_process(Header, Data)).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Arg1, Arg2) ->
    gen_server:start_link(?MODULE, [Arg1, Arg2], []).

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
init([ODWatcher, Idx]) ->
    ?info([?MODULE, init, "Starting OD worker.",
           {idx, Idx}, {od_watcher, ODWatcher}, {worker, self()}
          ]),
    {ok, #s{watcher = ODWatcher, idx = Idx}}.

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
handle_call(?worker_set_types(Types), _From, State) ->
    ?debug([?MODULE, handle_call, worker_set_types, {types, Types}]),
    {reply, ok, State#s{types = Types}};
handle_call(Request, _From, State) ->
    ?debug([?MODULE, handle_call, {req, Request}]),
    {reply, ok, State}.

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
handle_cast(
  ?worker_data_process(Header, Data),
  State = #s{templates = Templates0, unprocessed = Unprocessed0, types = Types}
 ) ->
%%    ?debug([?MODULE, handle_cast, worker_data_process, {header, Header}, {data, Data}]),
    case whistle_misc:duration({netsink, packet_data, [netsink:header_version(Header), Data]}) of %% netsink:packet_data(netsink:header_version(Header), Data)) of
        {ok, DataFlowSet} ->
            ?debug([?MODULE, handle_cast, worker_data_process, {data_packets_len, length(DataFlowSet)}]),
            case netsink:process_flowsets(Templates0, DataFlowSet) of
                {ok, Templates1, NetFlowData0} ->
                    ?debug(
                       [?MODULE, handle_cast, worker_data_process,
                        {templates, Templates1}, {data, NetFlowData0}]
                      ),
                    Unprocessed1 = lists:concat([Unprocessed0, NetFlowData0]),
                    {Processed, Unprocessed2} = whistle_misc:duration({netsink, apply_templates, [Templates1, Unprocessed1]}), %% netsink:apply_templates(Templates1, Unprocessed1),
                    Fun = fun() ->
                                  lists:map(
                                    fun ({ID, Length, DataToFormat}) ->
                                            netsink:format_data(Types, ID, Length, DataToFormat)
                                    end, Processed)
                          end,
                    whistle_misc:duration({Fun, []}),
                    {noreply, State#s{templates = Templates1, unprocessed = Unprocessed2}};
                {error, Reason} ->
                    ?error([?MODULE, handle_cast, worker_data_process, {error, Reason}])
            end;
        {error, Reason} ->
            ?error([?MODULE, handle_cast, worker_data_process, {error, Reason}]),
            {noreply, State}
    end;
handle_cast(Msg, State) ->
    ?debug([?MODULE, handle_cast, {msg, Msg}]),
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
handle_info(Info, State) ->
    ?debug([?MODULE, handle_info, {info, Info}]),
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
terminate(Reason, _State) ->
    ?debug([?MODULE, terminate, {reason, Reason}]),
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
