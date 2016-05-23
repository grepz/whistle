%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(netsink_udp).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("whistle_misc/include/logging.hrl").
-include_lib("netsink/include/netsink.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          port :: inet:port_number(),
          ip_addr :: inet:ip_address(),
          sock :: pid(),
          router :: pid()
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
start_link(IPaddr, Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {IPaddr, Port}, []).

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
init({IPaddr, Port}) ->
    ?info([?MODULE, init, "Starting netsink UDP server", {ip_addr, IPaddr}, {port, Port}]),
    {ok, Socket} = gen_udp:open(Port, [{ip, IPaddr}, binary]),
    ok = inet:setopts(Socket, [{active, once}]),
    {ok, #state{port = Port, ip_addr = IPaddr, sock = Socket}}.

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
handle_call(Request, From, State) ->
    ?debug([?MODULE, handle_call, {request, Request}, {from, From}]),
    Reply = unknown,
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
handle_cast(Msg, State) ->
    ?debug([?MODULE, handle_cast, {message, Msg}]),
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
handle_info({udp, Socket, Addr, _Port, Packet}, State = #state{sock = Socket}) ->
    case netsink:packet_header(Packet) of
        {ok, Header, Data} ->
            ok = route_packet(Addr, Header, Data);
        {error, Error} ->
            ?error([?MODULE, handle_info, udp, {error, Error}])
    end,
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info(_, State) ->
    ?error([?MODULE, handle_info, unknown_call]),
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

route_packet(Addr, Header, Data) ->
    SrcID = netsink:header_src_id(Header),
    Ver = netsink:header_version(Header),
    ?debug([?MODULE, route_packet, {version, Ver}, {src_id, SrcID}, {addr, Addr}]),
    ok = gen_server:cast(netsink_od_router, ?route_packet({SrcID, Addr}, Header, Data)).
