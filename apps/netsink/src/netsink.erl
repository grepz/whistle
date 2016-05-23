-module(netsink).

-export([netflow_rec_pp/2]).

-export([packet_header/1, header_version/1, header_src_id/1, packet_data/2]).

-include_lib("netsink/include/netflow.hrl").
-include_lib("whistle_misc/include/logging.hrl").

%% 1. An Export Packet consisting of interleaved Template, Data, and
%%    Options Template FlowSets.  Example: a newly created Template is
%%    exported as soon as possible.  So if there is already an Export
%%    Packet with a Data FlowSet that is being prepared for export, the
%%    Template and Option FlowSets are also interleaved with this
%%    information, subject to availability of space.

%% Export packet:
%% +--------+--------------------------------------------------------+
%% |        | +----------+ +---------+     +-----------+ +---------+ |
%% | Packet | | Template | | Data    |     | Options   | | Data    | |
%% | Header | | FlowSet  | | FlowSet | ... | Template  | | FlowSet | |
%% |        | |          | |         |     | FlowSet   | |         | |
%% |        | +----------+ +---------+     +-----------+ +---------+ |
%% +--------+--------------------------------------------------------+

%% 2. An Export Packet consisting entirely of Data FlowSets.  Example:
%%    after the appropriate Template Records have been defined and
%%    transmitted to the NetFlow Collector device, the majority of
%%    Export Packets consists solely of Data FlowSets.

%% Export Packet:
%% +--------+----------------------------------------------+
%% |        | +---------+     +---------+      +---------+ |
%% | Packet | | Data    | ... | Data    | ...  | Data    | |
%% | Header | | FlowSet | ... | FlowSet | ...  | FlowSet | |
%% |        | +---------+     +---------+      +---------+ |
%% +--------+----------------------------------------------+



%% ##############
%% Header Format
%% ##############

%% The Packet Header format is specified as:

%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |       Version Number          |            Count              |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                           sysUpTime                           |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                           UNIX Secs                           |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                       Sequence Number                         |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                        Source ID                              |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

%% Version
%%       Version of Flow Record format exported in this packet.  The
%%       value of this field is 9 for the current version.

%% Count
%%       The total number of records in the Export Packet, which is the
%%       sum of Options FlowSet records, Template FlowSet records, and
%%       Data FlowSet records.

%% sysUpTime
%%       Time in milliseconds since this device was first booted.

%% UNIX Secs
%%       Time in seconds since 0000 UTC 1970, at which the Export Packet
%%       leaves the Exporter.

%% Sequence Number
%%       Incremental sequence counter of all Export Packets sent from
%%       the current Observation Domain by the Exporter.  This value
%%       MUST be cumulative, and SHOULD be used by the Collector to
%%       identify whether any Export Packets have been missed.

%% Source ID
%%       A 32-bit value that identifies the Exporter Observation Domain.
%%       NetFlow Collectors SHOULD use the combination of the source IP
%%       address and the Source ID field to separate different export
%%       streams originating from the same Exporter.



%% ########################
%% Template FlowSet Format
%% ########################

%%    The format of the Template FlowSet is as follows:

%%     0                   1                   2                   3
%%     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |       FlowSet ID = 0          |          Length               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |      Template ID 256          |         Field Count           |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |        Field Type 1           |         Field Length 1        |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |        Field Type 2           |         Field Length 2        |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |             ...               |              ...              |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |        Field Type N           |         Field Length N        |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |      Template ID 257          |         Field Count           |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |        Field Type 1           |         Field Length 1        |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |        Field Type 2           |         Field Length 2        |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |             ...               |              ...              |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |        Field Type M           |         Field Length M        |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |             ...               |              ...              |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |        Template ID K          |         Field Count           |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |             ...               |              ...              |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

%%    FlowSet ID
%%          FlowSet ID value of 0 is reserved for the Template FlowSet.

%%    Length
%%          Total length of this FlowSet.  Because an individual Template
%%          FlowSet MAY contain multiple Template Records, the Length value
%%          MUST be used to determine the position of the next FlowSet
%%          record, which could be any type of FlowSet.  Length is the sum
%%          of the lengths of the FlowSet ID, the Length itself, and all
%%          Template Records within this FlowSet.

%%    Template ID
%%          Each of the newly generated Template Records is given a unique
%%          Template ID.  This uniqueness is local to the Observation
%%          Domain that generated the Template ID.  Template IDs 0-255 are
%%          reserved for Template FlowSets, Options FlowSets, and other
%%          reserved FlowSets yet to be created.  Template IDs of Data
%%          FlowSets are numbered from 256 to 65535.

%%    Field Count
%%          Number of fields in this Template Record.   Because a Template
%%          FlowSet usually contains multiple Template Records, this field
%%          allows the Collector to determine the end of the current
%%          Template Record and the start of the next.

%%    Field Type
%%          A numeric value that represents the type of the field.  Refer
%%          to the "Field Type Definitions" section.

%%    Field Length
%%          The length of the corresponding Field Type, in bytes.  Refer to
%%          the "Field Type Definitions" section.



%% ####################
%% Data FlowSet Format
%% ####################

%%    The format of the Data FlowSet is as follows:

%%     0                   1                   2                   3
%%     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   FlowSet ID = Template ID    |          Length               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   Record 1 - Field Value 1    |   Record 1 - Field Value 2    |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   Record 1 - Field Value 3    |             ...               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   Record 2 - Field Value 1    |   Record 2 - Field Value 2    |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   Record 2 - Field Value 3    |             ...               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   Record 3 - Field Value 1    |             ...               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |              ...              |            Padding            |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

%%    Data FlowSet Field Descriptions

%%    FlowSet ID = Template ID
%%          Each Data FlowSet is associated with a FlowSet ID.  The FlowSet
%%          ID maps to a (previously generated) Template ID.  The Collector
%%          MUST use the FlowSet ID to find the corresponding Template
%%          Record and decode the Flow Records from the FlowSet.

%%    Length
%%          The length of this FlowSet.  Length is the sum of the lengths
%%          of the FlowSet ID, Length itself, all Flow Records within this
%%          FlowSet, and the padding bytes, if any.

%%    Record N - Field Value M
%%          The remainder of the Data FlowSet is a collection of Flow Data
%%          Record(s), each containing a set of field values.  The Type and
%%          Length of the fields have been previously defined in the
%%          Template Record referenced by the FlowSet ID or Template ID.

%%    Padding
%%          The Exporter SHOULD insert some padding bytes so that the
%%          subsequent FlowSet starts at a 4-byte aligned boundary.  It is
%%          important to note that the Length field includes the padding
%%          bytes.  Padding SHOULD be using zeros.

%% Interpretation of the Data FlowSet format can be done only if the
%% Template FlowSet corresponding to the Template ID is available at the
%% Collector.



%% ################################
%% Options Template FlowSet Format
%% ################################

%%    The Options Template Record (and its corresponding Options Data
%%    Record) is used to supply information about the NetFlow process
%%    configuration or NetFlow process specific data, rather than supplying
%%    information about IP Flows.

%%    For example, the Options Template FlowSet can report the sample rate
%%    of a specific interface, if sampling is supported, along with the
%%    sampling method used.

%%    The format of the Options Template FlowSet follows.

%%     0                   1                   2                   3
%%     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |       FlowSet ID = 1          |          Length               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |         Template ID           |      Option Scope Length      |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |        Option Length          |       Scope 1 Field Type      |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |     Scope 1 Field Length      |               ...             |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |     Scope N Field Length      |      Option 1 Field Type      |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |     Option 1 Field Length     |             ...               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |     Option M Field Length     |           Padding             |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

%%    Options Template FlowSet Field Definitions

%%    FlowSet ID = 1
%%          A FlowSet ID value of 1 is reserved for the Options Template.

%%    Length
%%          Total length of this FlowSet.  Each Options Template FlowSet
%%          MAY contain multiple Options Template Records.  Thus, the
%%          Length value MUST be used to determine the position of the next
%%          FlowSet record, which could be either a Template FlowSet or
%%          Data FlowSet.
%%          Length is the sum of the lengths of the FlowSet ID, the Length
%%          itself, and all Options Template Records within this FlowSet
%%          Template ID.

%%    Template ID
%%          Template ID of this Options Template.  This value is greater
%%          than 255.

%%    Option Scope Length
%%          The length in bytes of any Scope field definition contained in
%%          the Options Template Record (The use of "Scope" is described
%%          below).

%%    Option Length
%%          The length (in bytes) of any options field definitions
%%          contained in this Options Template Record.

%%    Scope 1 Field Type
%%          The relevant portion of the Exporter/NetFlow process to which
%%          the Options Template Record refers.
%%          Currently defined values are:
%%             1 System
%%             2 Interface
%%             3 Line Card
%%             4 Cache
%%             5 Template
%%          For example, the NetFlow process can be implemented on a per-
%%          interface basis, so if the Options Template Record were
%%          reporting on how the NetFlow process is configured, the Scope
%%          for the report would be 2 (interface).  The associated
%%          interface ID would then be carried in the associated Options
%%          Data FlowSet.  The Scope can be limited further by listing
%%          multiple scopes that all must match at the same time.  Note
%%          that the Scope fields always precede the Option fields.

%%    Scope 1 Field Length
%%          The length (in bytes) of the Scope field, as it would appear in
%%          an Options Data Record.

%%    Option 1 Field Type
%%          A numeric value that represents the type of field that would
%%          appear in the Options Template Record.  Refer to the Field Type
%%          Definitions section.

%%    Option 1 Field Length
%%          The length (in bytes) of the Option field.
%%    Padding
%%          The Exporter SHOULD insert some padding bytes so that the
%%          subsequent FlowSet starts at a 4-byte aligned boundary.  It is
%%          important to note that the Length field includes the padding
%%          bytes.  Padding SHOULD be using zeros.



%% ###########################
%% Options Data Record Format
%% ###########################

%%    The Options Data Records are sent in Data FlowSets, on a regular
%%    basis, but not with every Flow Data Record.  How frequently these
%%    Options Data Records are exported is configurable.  See the
%%    "Templates Management" section for more details.

%%    The format of the Data FlowSet containing Options Data Records
%%    follows.

%%     0                   1                   2                   3
%%     0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |    FlowSet ID = Template ID   |          Length               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   Record 1 - Scope 1 Value    |Record 1 - Option Field 1 Value|
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |Record 1 - Option Field 2 Value|             ...               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   Record 2 - Scope 1 Value    |Record 2 - Option Field 1 Value|
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |Record 2 - Option Field 2 Value|             ...               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |   Record 3 - Scope 1 Value    |Record 3 - Option Field 1 Value|
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |Record 3 - Option Field 2 Value|             ...               |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%    |              ...              |            Padding            |
%%    +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

%%    Options Data Records of the Data FlowSet Field Descriptions

%%    FlowSet ID = Template ID
%%          A FlowSet ID precedes each group of Options Data Records within
%%          a Data FlowSet.  The FlowSet ID maps to a previously generated
%%          Template ID corresponding to this Options Template Record.  The
%%          Collector MUST use the FlowSet ID to map the appropriate type
%%          and length to any field values that follow.
%%    Length
%%          The length of this FlowSet. Length is the sum of the lengths of
%%          the FlowSet ID, Length itself, all the Options Data Records
%%          within this FlowSet, and the padding bytes, if any.

%%    Record N - Option Field M Value
%%          The remainder of the Data FlowSet is a collection of Flow
%%          Records, each containing a set of scope and field values.  The
%%          type and length of the fields were previously defined in the
%%          Options Template Record referenced by the FlowSet ID or
%%          Template ID.

%%    Padding
%%          The Exporter SHOULD insert some padding bytes so that the
%%          subsequent FlowSet starts at a 4-byte aligned boundary.  It is
%%          important to note that the Length field includes the padding
%%          bytes.  Padding SHOULD be using zeros.

%%    The Data FlowSet format can be interpreted only if the Options
%%    Template FlowSet corresponding to the Template ID is available at the
%%    Collector.


packet_header(<<?NETFLOW_V9:16/big-unsigned-integer, Rest/binary>>) ->
    decode_header_v9(Rest);
packet_header(<<?NETFLOW_V5:16/big-unsigned-integer, Rest/binary>>) ->
    decode_header_v5(Rest).

packet_data(?NETFLOW_V9, Data) ->
    parse_data_v9(Data, []);
packet_data(?NETFLOW_V5, Data) ->
    parse_data_v5(Data).

header_src_id(#netflow_export_header{src_id = Ver}) -> Ver.

header_version(#netflow_export_header{ver = Ver}) -> Ver.

%% decode(<<?NETFLOW_V9:16/big-unsigned-integer, Rest/binary>>) ->
%%     decode_v9(Rest);
%% decode(<<?NETFLOW_V5:16/big-unsigned-integer, Rest/binary>>) ->
%%     decode_v5(Rest);
%% decode(_) ->
%%     {error, packet_header_unknown}.


%% io_lib_pretty:print(Rec, fun netsink:netflow_rec_pp/2)

netflow_rec_pp(netflow_export_header, 6) ->
    record_info(fields, netflow_export_header);
netflow_rec_pp(netflow_export_packet, 2) ->
    record_info(fields, netflow_export_packet);
netflow_rec_pp(_, _) -> no.

%% Internal

decode_header_v9(
  <<Count:16/big-unsigned-integer, SysUpTime:32/big-unsigned-integer,
    Timestamp:32/big-unsigned-integer, SeqNum:32/big-unsigned-integer,
    SrcID:32/big-unsigned-integer, Data/binary>>
 ) ->
    Header =
        #netflow_export_header{
           ver = ?NETFLOW_V9,
           count = Count,
           uptime = SysUpTime,
           timestamp = Timestamp,
           seq_num = SeqNum,
           src_id = SrcID
          },
    {ok, Header, Data};
decode_header_v9(_) ->
    {error, unknown_header_format}.

decode_header_v5(_) ->
    {error, unimplemented}.

parse_data_v9(
  <<FlowSetID:16/big-unsigned-integer,
    PacketLength:16/big-unsigned-integer, Data/binary>>, Acc
 ) ->
    ?debug([?MODULE, parse_data_v9, {length, PacketLength}, {flowset_id, FlowSetID}]),
    %% Deduct FlowSetID and Length parameters from packet length
    case parse_packet_data(PacketLength - 4, FlowSetID, Data) of
        {ok, FlowSetData, Rest} -> parse_data_v9(Rest, [FlowSetData | Acc]);
        Else -> Else
    end;
parse_data_v9(<<>>, Acc) ->
    {ok, Acc}.

parse_packet_data(Len, FlowSetID, Data) when is_integer(Len) andalso Len > 0 ->
    case Data of
        <<PacketData:Len/binary, Rest/binary>> -> {ok, {FlowSetID, PacketData}, Rest};
        _ -> {error, packet_binary_format}
    end;
parse_packet_data(_, _, _) ->
    {error, top_binary_format}.


parse_data_v5(_) ->
    {error, unimplemented}.
