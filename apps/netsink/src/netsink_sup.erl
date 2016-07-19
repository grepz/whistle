-module(netsink_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    NetsinkPort = whistle_misc:get_app_env(port, 2055),
    NetsinkIP = whistle_misc:get_app_env(ip_addr, {0,0,0,0}),
    NetsinkODPoolSz = whistle_misc:get_app_env(od_pool_sz, 10),
    NetsinkUserTypes = whistle_misc:get_app_env(netflow_user_types, undefined),
    NetsinkTypesBind = whistle_misc:get_app_env(netflow_types_bind, undefined),
    {ok, {{one_for_one, 10, 60},
           [
            ?CHILD(netsink_types_mgr, worker, [NetsinkUserTypes, NetsinkTypesBind]),
            ?CHILD(netsink_udp, worker, [NetsinkIP, NetsinkPort]),
            ?CHILD(netsink_od_sup, supervisor, [NetsinkODPoolSz])
           ]
         }}.

%%====================================================================
%% Internal functions
%%====================================================================
