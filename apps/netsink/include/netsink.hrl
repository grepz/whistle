-ifndef(include_netsink_hrl).
-define(include_netsink_hrl, true).

-define(route_packet(ODID, Header, Data), {route_packet, ODID, Header, Data}).

-endif. % include_netsink_hrl
