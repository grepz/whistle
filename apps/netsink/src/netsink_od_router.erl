%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 17 May 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(netsink_od_router).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_worker/2, stop_watcher/0]).
-export([is_worker_binded/1, worker_status/1]).
-export([renew_workers_bind_types/1, route_packet/3]).

-define(SERVER, ?MODULE).

-define(od_watcher_shutdown(Reason), {od_watcher_shutdown, Reason}).
-define(renew_workers_bind_types(Types), {renew_workers_bind_types, Types}).
-define(route_packet(ODID, Header, Data), {route_packet, ODID, Header, Data}).

-include_lib("whistle_misc/include/logging.hrl").
-include_lib("netsink/include/netsink.hrl").

-record(od_worker, {pid = undefined, status = ready, od = undefined}).

-record(s, {
          workers_sup :: pid(),
          od_workers_queue :: queue:queue(#od_worker{}),
          od_workers_binded :: dict:dict({integer(), inet:ip_address()}, #od_worker{}),
          types :: list:list(term()),
          pool_sz :: non_neg_integer()
         }
       ).

%%%===================================================================
%%% API
%%%===================================================================

-spec renew_workers_bind_types(list:list(term())) -> ok | {error, list:list(term())}.
renew_workers_bind_types(Types) ->
    gen_server:call(?MODULE, ?renew_workers_bind_types(Types)).

-spec route_packet(ODID :: {term(), term()}, Header :: binary(), Data :: binary()) -> ok.
route_packet(ODID, Header, Data) ->
    ok = gen_server:cast(?MODULE, ?route_packet(ODID, Header, Data)).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Arg1, Arg2) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Arg1, Arg2], []).

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
init([ODPoolSz, Sup]) ->
    ?info([?MODULE, init, "Starting OD watcher process", {od_pool_sz, ODPoolSz}, {workers_sup, Sup}]),
    {ok, Types} = netsink_types_mgr:get_types(),
    false = process_flag(trap_exit, true),
    Workers = start_worker_pool(ODPoolSz, Sup),
    {ok, #s{
            workers_sup = Sup,
            od_workers_queue = queue:from_list(Workers),
            od_workers_binded = dict:new(),
            types = Types,
            pool_sz = ODPoolSz
           }}.

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
handle_call(?od_watcher_shutdown(Reason), From, State = #s{}) ->
    ?info([?MODULE, handle_call, ?od_watcher_shutdown(Reason), {from, From}]),
    %% NOTE: No proper shutdown strategy is defined for workers right now
    %% Kill everyting that is linked to a process
    {stop, not_normal, shutdown_ok, State};
handle_call(
  ?renew_workers_bind_types(Types), _From,
  State = #s{od_workers_binded = BindedWorkers}
 ) ->
    ?debug([?MODULE, handle_call, renew_workers_bind_types, {types, Types}]),
    Result =
        lists:fold(
          fun (#od_worker{pid = WorkerPid}, Acc) ->
                  case netsink_od_worker:set_types(WorkerPid, Types) of
                      ok ->
                          ?debug(
                             [?MODULE, handle_call, renew_workers_bind_types, ok,
                              {pid, WorkerPid}]
                            ),
                          Acc;
                      {error, Reason} ->
                          ?error(
                             [?MODULE, handle_call, renew_workers_bind_types,
                              {error, Reason}]
                            ),
                          [{WorkerPid, Reason} | Acc]
                  end
          end, [], dict:to_list(BindedWorkers)),
    case Result of
        [] ->
            {reply, ok, State#s{types = Types}};
        _ ->
            %% TODO: Should do some sort of transaction handling, if error occured,
            %% revert worker bind types to its old state
            {reply, {error, Result}, State#s{types = Types}}
    end;
handle_call(Request, From, State) ->
    ?debug([?MODULE, handle_call, {req, Request}, {from, From}]),
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
handle_cast(
  ?route_packet(ODID, Header, Data),
  State = #s{
             od_workers_binded = Binded0,
             od_workers_queue = Queue0,
             pool_sz = PoolSz,
             types = Types,
             workers_sup = WorkersSup
            }
 ) ->
    ?debug([?MODULE, handle_cast, route_packet, {od_id, ODID}]),
    case dict:find(ODID, Binded0) of
        {ok, ODWorker} ->
            ok = push_to_worker(ODWorker, Header, Data),
            {noreply, State};
        error ->
            case ensure_bind_worker(ODID, Types, Queue0, Binded0, PoolSz, WorkersSup) of
                {ok, {Queue1, Binded1, ODWorker}} ->
                    ok = push_to_worker(ODWorker, Header, Data),
                    {noreply, State#s{
                                od_workers_binded = Binded1,
                                od_workers_queue = Queue1
                               }};
                {error, Reason} ->
                    ?error([?MODULE, handle_cast, route_packet, dropping, {error, Reason}]),
                    {noreply, State}
            end
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
handle_info(
  {'EXIT', WorkerPidExited, Reason},
  State = #s{
     od_workers_queue = WorkersQueue0,
     od_workers_binded = WorkersBinded0
    }
 ) ->
    ?info([?MODULE, handle_info, worker_exit, {worker_pid, WorkerPidExited}, {reason, Reason}]),
    case cleanup_pool_queue(WorkerPidExited, WorkersQueue0) of
        {true, WorkersQueue1} ->
            {noreply, State#s{od_workers_queue = WorkersQueue1}};
        {false, _} ->
            WorkersBinded1 =
                dict:fold(
                  fun (_Bind, Worker = #od_worker{pid = WorkerPid}, Acc) ->
                          case WorkerPid == WorkerPidExited of
                              true -> Acc;
                              false -> [Worker | Acc]
                          end
                  end, dict:new(), WorkersBinded0),
            {noreply, State#s{od_workers_binded = WorkersBinded1}}
    end;
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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%-----`---------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_worker(Sup, Idx) ->
    ODWatcherPid = self(),
    {ok, ChildPid} = supervisor:start_child(Sup, [ODWatcherPid, Idx]),
    true = erlang:link(ChildPid),
    ChildPid.

stop_watcher() ->
    gen_server:call(?MODULE, ?od_watcher_shutdown(asking_to_stop)).

is_worker_binded(#od_worker{status = binded}) -> true;
is_worker_binded(_) -> false.

worker_status(#od_worker{status = Status}) -> Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_worker_pool(PoolSz, Sup) ->
    start_worker_pool(PoolSz, Sup, []).

start_worker_pool(0, _Sup, Workers) -> Workers;
start_worker_pool(PoolSz, Sup, Workers) ->
    WorkerPid = start_worker(Sup, PoolSz),
    Worker = #od_worker{pid = WorkerPid},
    start_worker_pool(PoolSz - 1, Sup, [Worker | Workers]).

cleanup_pool_queue(Pid, Queue) ->
    List0 = queue:to_list(Queue),
    {Found, List1} =
        lists:foldl(
          fun (Worker = #od_worker{pid = WorkerPid}, {Cond, Acc}) ->
                  case WorkerPid == Pid of
                      true -> {true, Acc};
                      false -> {Cond, [Worker | Acc]}
                  end
          end, {false, []}, List0),
    case Found of
        true -> {Found, queue:from_list(List1)};
        false -> {Found, Queue}
    end.

ensure_bind_worker(ODID, Types, Queue0, Binded0, PoolSz, WorkersSup) ->
    case bind_worker(ODID, Types, Queue0, Binded0) of
        {ok, Result} -> {ok, Result};
        {error, wpool_empty} ->
            Queue1 = queue:from_list(start_worker_pool(PoolSz, WorkersSup)),
            {_Result, _Details} = bind_worker(ODID, Types, Queue1, Binded0);
        Else -> Else
    end.

bind_worker(ODID, Types, Queue0, Binded0) ->
    case queue:out(Queue0) of
        {{value, ODWorker0 = #od_worker{pid = WorkerPid}}, Queue1} ->
            case netsink_od_worker:set_types(WorkerPid, Types) of
                ok ->
                    ODWorker1 = ODWorker0#od_worker{od = ODID},
                    Binded1 = dict:store(ODID, ODWorker1, Binded0),
                    {ok, {Queue1, Binded1, ODWorker1}};
                {error, Reason} ->
                    {error, {set_types, Reason}}
            end;
        {empty, _} ->
            {error, wpool_empty}
    end.

push_to_worker(#od_worker{pid = Pid}, Header, Data) ->
    ok = netsink_od_worker:data_process(Pid, Header, Data).
