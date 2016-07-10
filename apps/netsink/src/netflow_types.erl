%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(netflow_types).

%% API
-export([integer/1, integer/2, ip_addr/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

integer(Data) ->
    integer(big, Data).

integer(Endianess, Data) ->
    binary:decode_unsigned(Data, Endianess).

ip_addr(<<X1, X2, X3, X4>>) ->
    {X1, X2, X3, X4}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
