-module(netsink).

-export([netflow_rec_pp/2]).

-export([packet_header/1, header_version/1, header_src_id/1, packet_data/2]).

-export([process_flowsets/2]).

-include_lib("netsink/include/netflow.hrl").
-include_lib("whistle_misc/include/logging.hrl").

packet_header(<<?NETFLOW_V9:16/big-unsigned-integer, Rest/binary>>) ->
    decode_header_v9(Rest);
packet_header(<<?NETFLOW_V5:16/big-unsigned-integer, Rest/binary>>) ->
    decode_header_v5(Rest).

packet_data(?NETFLOW_V9, Data) ->
    parse_packet_data_v9(Data, []);
packet_data(?NETFLOW_V5, Data) ->
    parse_packet_data_v5(Data).

header_src_id(#netflow_export_header{src_id = Ver}) -> Ver.

header_version(#netflow_export_header{ver = Ver}) -> Ver.

%% decode(<<?NETFLOW_V9:16/big-unsigned-integer, Rest/binary>>) ->
%%     decode_v9(Rest);
%% decode(<<?NETFLOW_V5:16/big-unsigned-integer, Rest/binary>>) ->
%%     decode_v5(Rest);
%% decode(_) ->
%%     {error, packet_header_unknown}.

process_flowsets(Templates, FlowSets) ->
    process_flowsets(Templates, FlowSets, []).

%% io_lib_pretty:print(Rec, fun netsink:netflow_rec_pp/2)

netflow_rec_pp(netflow_export_header, 6) ->
    record_info(fields, netflow_export_header);
netflow_rec_pp(netflow_export_packet, 2) ->
    record_info(fields, netflow_export_packet);
netflow_rec_pp(_, _) -> no.

%% Internal

update_templates(Templates, []) -> Templates;
update_templates(OldTemplates, [NewTemplate | NewTemplates]) ->
    TemplatesUpdated =
        case lists:keytake(NewTemplate#template_rec.id, #template_rec.id, OldTemplates) of
            false -> [NewTemplate | OldTemplates];
            {value, _, TemplatesCut} -> [NewTemplate | TemplatesCut]
        end,
    update_templates(TemplatesUpdated, NewTemplates).

process_flowsets(Templates0, [FlowSet | FlowSets], ParsedData) ->
    case decode_flowset(FlowSet) of
        {ok, {template, TemplateFlowSet}} ->
            Templates1 = update_templates(Templates0, TemplateFlowSet),
            process_flowsets(Templates1, FlowSets, ParsedData);
        {ok, {data, TemplateID, RecordsNum, Data}} ->
            {ok, NetFlowData} = apply_template(Templates0, TemplateID, RecordsNum, Data),
            process_flowsets(Templates0, FlowSets, [NetFlowData | ParsedData]);
        Else -> Else
    end;
process_flowsets(Templates, [], ParsedData) ->
    {ok, Templates, ParsedData}.

apply_template([], _, _, _) ->
    %% TODO: If theres no templates to use then data should be stored until
    %% application receives necessary templates
    {ok, []};
apply_template(_Templates, TemplateID, RecordsNum, _Data) ->
    ?debug([?MODULE, apply_template, {template_id, TemplateID}, {records_num, RecordsNum}]),
    {ok, []}.

decode_flowset({?NETFLOW_FLOWSET_TEMPLATE_ID, RecordsNum, Template}) ->
    decode_template(RecordsNum, Template);
decode_flowset({FlowSetID, RecordsNum, Data})
  when is_integer(FlowSetID) andalso FlowSetID > ?NETFLOW_FLOWSET_MAX_RESERVED_ID ->
    decode_data(FlowSetID, RecordsNum, Data);
decode_flowset({FlowSetID, _, _}) ->
    {error, {unknown_flowset_id, FlowSetID}}.

decode_template(RecordsNumTotal, TemplateData)
  when is_integer(RecordsNumTotal) andalso RecordsNumTotal > 0 ->
    Templates = decode_template_iter(TemplateData, []),
    {ok, {template, Templates}}.

decode_template_iter(<<>>, Acc) -> Acc;
decode_template_iter(
  <<TemplateID:16/big-unsigned-integer, FieldsCnt:16/big-unsigned-integer, Rest0/binary>>, Acc
 ) ->
    {TemplateFieldRecs, Rest1} = decode_single_template(FieldsCnt, Rest0, []),
    TemplateRec =
        #template_rec{
           id = TemplateID,
           fields = TemplateFieldRecs
          },
    decode_template_iter(Rest1, [TemplateRec | Acc]).

decode_single_template(0, Rest, Acc) -> {Acc, Rest};
decode_single_template(
  FieldsCnt,
  <<FieldType:16/big-unsigned-integer, FieldLength:16/big-unsigned-integer, Rest/binary>>,
  Acc
 ) ->
    TemplateRec =
        #template_field_rec{
           length = FieldLength,
           type = FieldType
          },
    decode_single_template(FieldsCnt - 1, Rest, [TemplateRec | Acc]).

decode_data(FlowSetID, RecordsNum, Data) ->
    {ok, {data, FlowSetID, RecordsNum, Data}}.

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

parse_packet_data_v9(
  <<FlowSetID:16/big-unsigned-integer,
    PacketLength:16/big-unsigned-integer, Data/binary>>, Acc
 ) ->
    ?debug([?MODULE, parse_packet_data_v9, {length, PacketLength}, {flowset_id, FlowSetID}]),
    %% Deduct FlowSetID and Length parameters from packet length
    BytesNum = PacketLength - 4,
    case get_packet_data(BytesNum, FlowSetID, Data) of
        {ok, FlowSetData, Rest} -> parse_packet_data_v9(Rest, [FlowSetData | Acc]);
        Else -> Else
    end;
parse_packet_data_v9(<<>>, Acc) ->
    {ok, Acc}.

get_packet_data(Len, FlowSetID, Data) when is_integer(Len) andalso Len > 0 ->
    case Data of
        <<PacketData:Len/binary, Rest/binary>> ->
            %% Each record is 2 bytes long
            {ok, {FlowSetID, Len div 2, PacketData}, Rest};
        _ -> {error, packet_binary_format}
    end;
get_packet_data(_, _, _) ->
    {error, top_binary_format}.


parse_packet_data_v5(_) ->
    {error, unimplemented}.
