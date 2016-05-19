%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created :  1 May 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(whistle_misc).

%% API
-export([log/2, now/0, local_time_to_seconds/1, seconds_to_local_time/1,
         local_time_to_string/1, unix_time_to_string/1, get_app_env/2]).

-define(SECONDS1970, 62167219200). %% {{1970,1,1}, {0,0,0}}

-define(TRUNC_LEN, 64536).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

log(Lvl, LogMessage) ->
    {Fmt, Args} = log_format(LogMessage, [], []),
    log_message(Lvl, Fmt, Args).

now() ->
    {MegaS, S, _MicroS} = erlang:now(),
    MegaS * 1000000 + S.

local_time_to_seconds(LDT) ->
    calendar:datetime_to_gregorian_seconds(LDT) - ?SECONDS1970.

seconds_to_local_time(Seconds) ->
    BaseDate = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    GregorianSeconds = BaseDate + Seconds,
    {_Date, _Time} = calendar:gregorian_seconds_to_datetime(GregorianSeconds).

local_time_to_string({{Year, Mon, Day}, {Hour, Min, Sec}}) ->
    Format = "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
    lists:flatten(io_lib:format(Format, [Year, Mon, Day, Hour, Min, Sec])).

unix_time_to_string(Seconds) ->
    LocalTime = seconds_to_local_time(Seconds),
    local_time_to_string(LocalTime).

get_app_env(Opt, Default) ->
    {ok, App} = application:get_application(),
    case application:get_env(App, Opt) of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error       -> Default
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

log_message(Level, Fmt, Args) ->
    Out = lager_trunc_io:format(Fmt, Args, ?TRUNC_LEN),
    ok = lager:log(Level, self(), Out, []).

%% log_message(debug, Fmt, Args) ->
%%     ok = lager:debug(Fmt, Args);
%% log_message(error, Fmt, Args) ->
%%     ok = lager:error(Fmt, Args);
%% log_message(info, Fmt, Args) ->
%%     ok = lager:log(Fmt, Args);
%% log_message(_, _, _) -> ok.

log_format([], Fmt, Args) ->
    {string:join(lists:reverse(Fmt), ", "), lists:reverse(Args)};
log_format([{K,V} | Msg], Fmt, Args) ->
    log_format(Msg, ["~p: ~p" | Fmt], [V, K | Args]);
log_format([V | Msg], Fmt, Args) ->
    log_format(Msg, ["~p" | Fmt], [V | Args]).
