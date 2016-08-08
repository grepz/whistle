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
-export([integer/2, integer/3, ipv4_addr/2, none/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

integer(Length, Data) ->
    integer(Length, big, Data).

integer(_Length, Endianess, Data) ->
    binary:decode_unsigned(Data, Endianess).

ipv4_addr(4, <<X1, X2, X3, X4>>) -> {X1, X2, X3, X4};
ipv4_addr(_, Data) -> Data.

none(_, Data) -> Data.


%%%===================================================================
%%% Internal functions
%%%===================================================================
