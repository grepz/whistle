-ifndef(include_netsink_netflow_hrl).
-define(include_netsink_netflow_hrl, true).

-define(NETFLOW_V9, 9).
-define(NETFLOW_V5, 5).

-define(NETFLOW_FLOWSET_TEMPLATE_ID, 0).
-define(NETFLOW_FLOWSET_OPTIONS_TEMPLATE_ID, 1).
-define(NETFLOW_FLOWSET_MAX_RESERVED_ID, 255).

%% Incoming counter with length N x 8 bits for the number of bytes associated
%% with an IP Flow. By default N is 4
-define(IN_BYTES, 1). % N
%% Incoming counter with length N x 8 bits for the number of packets
%% associated with an IP Flow. By default N is 4
-define(IN_PKTS, 2). % N
%% Number of Flows that were aggregated; by default N is 4
-define(FLOWS, 3). % N
%% IP protocol byte
-define(PROTOCOL, 4). % 1
%% Type of service byte setting when entering the incoming interface
-define(TOS, 5). % 1
%% TCP flags; cumulative of all the TCP flags seen in this Flow
-define(TCP_FLAGS, 6). % 1
%% TCP/UDP source port number (for example, FTP, Telnet, or equivalent)
-define(L4_SRC_PORT, 7). % 2
%% IPv4 source address
-define(IPV4_SRC_ADDR, 8). % 4
%% The number of contiguous bits in the source subnet mask (i.e., the mask in
%% slash notation)
-define(SRC_MASK, 9). % 1
%% Input interface index. By default N is 2, but higher values can be used
-define(INPUT_SNMP, 10). % N
%% TCP/UDP destination port number (for example, FTP, Telnet, or equivalent)
-define(L4_DST_PORT, 11). % 2
%% IPv4 destination address
-define(IPV4_DST_ADDR, 12). % 4
%% The number of contiguous bits in the destination subnet mask (i.e., the mask
%% in slash notation)
-define(DST_MASK, 13). % 1
%% Output interface index. By default N is 2, but higher values can be used.
-define(OUTPUT_SNMP, 14). % N
%% IPv4 address of the next-hop router
-define(IPV4_NEXT_HOP, 15). % 4
%% Source BGP autonomous system number where N could be 2 or 4. By default N is 2
-define(SRC_AS, 16).  % N
%% Destination BGP autonomous system number where N could be 2 or 4. By default N is 2
-define(DST_AS, 17). % N
%% Next-hop router's IP address in the BGP domain
-define(BGP_IPV4_NEXT_HOP, 18). % 4
%% IP multicast outgoing packet counter with length N x 8 bits for packets
%% associated with the IP Flow. By default N is 4
-define(MUL_DST_PKTS, 19). % N
%% IP multicast outgoing Octet (byte) counter with length N x 8 bits for the
%% number of bytes associated with the IP Flow. By default N is 4
-define(MUL_DST_BYTES, 20). % N
%% sysUptime in msec at which the last packet of this Flow was switched
-define(LAST_SWITCHED, 21). % 4
%% sysUptime in msec at which the first packet of this Flow was switched
-define(FIRST_SWITCHED, 22). %  4
%% Outgoing counter with length N x 8 bits for the number of bytes associated
%% with an IP Flow. By default N is 4
-define(OUT_BYTES, 23). % N
%% Outgoing counter with length N x 8 bits for the number of packets
%% associated with an IP Flow. By default N is 4
-define(OUT_PKTS, 24). % N
%% IPv6 source address
-define(IPV6_SRC_ADDR, 27). % 16
%% IPv6 destination address
-define(IPV6_DST_ADDR, 28). % 16
%% Length of the IPv6 source mask in contiguous bits
-define(IPV6_SRC_MASK, 29). % 1
%% Length of the IPv6 destination mask in contiguous bits
-define(IPV6_DST_MASK, 30). % 1
%% IPv6 flow label as per RFC 2460 definition
-define(IPV6_FLOW_LABEL, 31). % 3
%% Internet Control Message Protocol (ICMP) packet
%% type; reported as ICMP Type * 256 + ICMP code
-define(ICMP_TYPE, 32). % 2
%% Internet Group Management Protocol (IGMP) packet type
-define(MUL_IGMP_TYPE, 33). % 1
%% When using sampled NetFlow, the rate at which packets are sampled; for example,
%% a value of 100 indicates that one of every hundred packets is sampled
-define(SAMPLING_INTERVAL, 34). % 4
%% For sampled NetFlow platform-wide:
%% 0x01 deterministic sampling
%% 0x02 random sampling Use in connection with SAMPLING_INTERVAL
-define(SAMPLING_ALGORITHM, 35). % 1
%% Timeout value (in seconds) for active flow entries in the NetFlow cache
-define(FLOW_ACTIVE_TIMEOUT, 36). % 2
%% Timeout value (in seconds) for inactive Flow entries in the NetFlow cache
-define(FLOW_INACTIVE_TIMEOUT, 37). % 2
%% Type of Flow switching engine (route processor, linecard, etc...)
-define(ENGINE_TYPE, 38). % 1
%% ID number of the Flow switching engine
-define(ENGINE_ID, 39). % 1.
%% Counter with length N x 8 bits for the number
%% of bytes exported by the Observation Domain. By default N is 4
-define(TOTAL_BYTES_EXP, 40). % N
%% Counter with length N x 8 bits for the number of packets exported by
%% the Observation Domain. By default N is 4
-define(TOTAL_PKTS_EXP, 41). % N
%% Counter with length N x 8 bits for the number of Flows exported by the
%% Observation Domain. By default N is 4
-define(TOTAL_FLOWS_EXP, 42). % N
%% MPLS Top Label Type:
%% 0x00 UNKNOWN
%% 0x01 TE-MIDPT
%% 0x02 ATOM
%% 0x03 VPN
%% 0x04 BGP
%% 0x05 LDP
-define(MPLS_TOP_LABEL_TYPE, 46). % 1
%% Forwarding Equivalent Class corresponding to the MPLS Top Label
-define(MPLS_TOP_LABEL_IP_ADDR, 47). % 4
%% Identifier shown in "show flow-sampler"
-define(FLOW_SAMPLER_ID, 48). % 1
%% The type of algorithm used for sampling data:
%% 0x02 random sampling Use in connection with FLOW_SAMPLER_MODE
-define(FLOW_SAMPLER_MODE, 49). % 1
%% Packet interval at which to sample. Use in connection with FLOW_SAMPLER_MODE
-define(FLOW_SAMPLER_RANDOM_INTERVAL, 50). % 4
%% Type of Service byte setting when exiting outgoing interface
-define(DST_TOS, 55). % 1
%% Source MAC Address
-define(SRC_MAC, 56). % 6
%% Destination MAC Address
-define(DST_MAC, 57). % 6
%% Virtual LAN identifier associated with ingress interface
-define(SRC_VLAN, 58). % 2
%% Virtual LAN identifier associated with egress interface
-define(DST_VLAN, 59). % 2
%% Internet Protocol Version Set to 4 for IPv4, set to 6 for IPv6.
%% If not present in the template, then version 4 is assumed
-define(IP_PROTOCOL_VERSION, 60). % 1
%% Flow direction:
%% 0 - ingress flow
%% 1 - egress flow
-define(DIRECTION, 61). % 1
%% IPv6 address of the next-hop router
-define(IPV6_NEXT_HOP, 62). % 16
%% Next-hop router in the BGP domain
-define(BGP_IPV6_NEXT_HOP, 63). % 16
%% Bit-encoded field identifying IPv6 option headers found in the flow
-define(IPV6_OPTION_HEADERS, 64). % 4
%% MPLS label at position 1 in the stack
-define(MPLS_LABEL_1, 70). % 3
%% MPLS label at position 2 in the stack
-define(MPLS_LABEL_2, 71). % 3
%% MPLS label at position 3 in the stack
-define(MPLS_LABEL_3, 72). % 3
%% MPLS label at position 4 in the stack
-define(MPLS_LABEL_4, 73). % 3
%% MPLS label at position 5 in the stack
-define(MPLS_LABEL_5, 74). % 3
%% MPLS label at position 6 in the stack
-define(MPLS_LABEL_6, 75). % 3
%% MPLS label at position 7 in the stack
-define(MPLS_LABEL_7, 76). % 3
%% MPLS label at position 8 in the stack
-define(MPLS_LABEL_8, 77). % 3
%% MPLS label at position 9 in the stack
-define(MPLS_LABEL_9, 78). % 3
%% MPLS label at position 10 in the stack
-define(MPLS_LABEL_10, 79). % 3

%% The value field is a numeric identifier for the field type. The
%% following value fields are reserved for proprietary field types: 25,
%% 26, 43 to 45, 51 to 54, and 65 to 69.


-record(template_field_rec, {
          length :: non_neg_integer(),
          type :: non_neg_integer()
         }).

-record(template_rec, {
          id :: non_neg_integer(),
          fields :: [#template_field_rec{}]
         }).

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
