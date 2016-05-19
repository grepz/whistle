-module(netsink_od_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(PoolSz) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [PoolSz]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([PoolSz]) ->
    {ok, {{one_for_all, 10, 60},
           [
            ?CHILD(netsink_od_worker_sup, supervisor, []),
            ?CHILD(netsink_od_router, worker, [PoolSz, netsink_od_worker_sup])
           ]
         }}.

%%====================================================================
%% Internal functions
%%====================================================================
