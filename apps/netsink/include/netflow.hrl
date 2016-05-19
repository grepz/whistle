-ifndef(include_netsink_netflow_hrl).
-define(include_netsink_netflow_hrl, true).

-define(NETFLOW_V9, 9).
-define(NETFLOW_V5, 5).

-record(netflow_export_header, {
          ver :: integer(),
          count :: non_neg_integer(),
          uptime :: non_neg_integer(),
          timestamp :: non_neg_integer(),
          seq_num :: non_neg_integer(),
          src_id :: integer()
         }).

-record(netflow_export_packet, {
          header :: #netflow_export_header{},
          data :: term()
         }).

-endif. % include_netsink_netflow_hrl
