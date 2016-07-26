%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(netsink_types_mgr).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([reload_types_bind/0, reload_user_types/0, reload_types/0, get_types/0]).

-define(SERVER, ?MODULE).

-record(s, {
          user_types_cfg = undefined,
          types_bind_cfg = undefined,
          types = []
         }).

-include_lib("whistle_misc/include/logging.hrl").
-include_lib("netsink/include/netsink.hrl").

-define(get_types_timeout, 10000).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_types() -> {ok, [term()]}.
get_types() ->
    gen_server:call(?MODULE, ?get_types(), ?get_types_timeout).

-spec reload_types() -> ok | {error, term()}.
reload_types() ->
    case reload_user_types() of
        ok ->
            Result = reload_types_bind(),
            ?debug([?MODULE, reload_types, {result, Result}]),
            Result;
        Error ->
            ?debug([?MODULE, reload_types, {error, Error}]),
            Error
    end.

-spec reload_types_bind() -> {ok, [term()]} | {error, term()}.
reload_types_bind() ->
    gen_server:call(?MODULE, ?reload_types(types_bind)).

-spec reload_user_types() -> ok | {error, term()}.
reload_user_types() ->
    gen_server:call(?MODULE, ?reload_types(user_types)).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(UserTypes, TypesBind) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {UserTypes, TypesBind}, []).

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
init({UserTypes, TypesBind}) ->
    ?info([?MODULE, init, {user_types, UserTypes}, {types_bind, TypesBind}]),
    ok = netflow_types_reader:user_types(UserTypes),
    {ok, Types} = netflow_types_reader:types_bind(TypesBind),
    {ok, #s{types_bind_cfg = TypesBind, user_types_cfg = UserTypes, types = Types}}.

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
handle_call(
  ?reload_types(types_bind), _From, State = #s{types_bind_cfg = UserBindCfg}
 ) ->
    ?debug([?MODULE, handle_call, reload_types, types_bind]),
    Result =
        try
            {ok, _Terms} = netflow_types_reader:types_bind(UserBindCfg)
        catch Error:Reason ->
                ?error(
                   [?MODULE, handle_call, user_types, {error, Error},
                    {reason, Reason}, {stack, erlang:get_stacktrace()}]
                  ),
            {error, Reason}
        end,
    {reply, Result, State};
handle_call(
  ?reload_types(user_types), _From, State = #s{user_types_cfg = UserTypesCfg}
 ) ->
    ?debug([?MODULE, handle_call, reload_types, user_types]),
    Result =
        try
            ok = netflow_types_reader:user_types(UserTypesCfg)
        catch Error:Reason ->
                ?error(
                   [?MODULE, handle_call, user_types, {error, Error},
                    {reason, Reason}, {stack, erlang:get_stacktrace()}]
                  ),
            {error, Reason}
        end,
    {reply, Result, State};
handle_call(?reload_types(Type), _From, State) ->
    ?error([?MODULE, handle_call, reload_types, {unknown_type, Type}]),
    Reply = error,
    {reply, Reply, State};
handle_call(?get_types(), From, State = #s{types = Types}) ->
    ?debug([?MODULE, handle_call, get_types, {from, From}, {types, Types}]),
    {reply, {ok, Types}, State};
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
