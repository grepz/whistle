-module(netsink_od_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, temporary, 2000, Type, [I]}).

-include_lib("whistle_misc/include/logging.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    ?info([?MODULE, init, "Starting OD workers supervisor"]),
    SupFlags = {simple_one_for_one, 0, 1},
    {ok, {SupFlags, [
                     ?CHILD(netsink_od_worker, worker, [])
                    ]
         }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
