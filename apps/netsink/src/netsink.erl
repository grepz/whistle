-module(netsink).

-export([netflow_rec_pp/2]).

-export([packet_header/1, header_version/1, header_src_id/1, packet_data/2]).

-export([process_flowsets/2, apply_templates/2, apply_template/4]).

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

apply_templates(Templates, Data) ->
    apply_templates(Templates, Data, [], []).

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

process_flowsets(Templates, [], DataAcc) ->
    {ok, Templates, DataAcc};
process_flowsets(Templates0, [FlowSet | FlowSets], DataAcc) ->
    case decode_flowset(FlowSet) of
        {ok, {template, TemplateFlowSet}} ->
            Templates1 = update_templates(Templates0, TemplateFlowSet),
            process_flowsets(Templates1, FlowSets, DataAcc);
        {ok, {data, TemplateID, RecordsNum, Data}} ->
            %% {ok, NetFlowData} = apply_template(Templates0, TemplateID, RecordsNum, Data),
            process_flowsets(
              Templates0, FlowSets, [{data, TemplateID, RecordsNum, Data}|DataAcc]
             );
        {error, Error} ->
            ?error([?MODULE, process_flowsets, {flowset, FlowSet}, {error, Error}]),
            process_flowsets(Templates0, FlowSets, DataAcc)
    end.

apply_template(Templates, TemplateID, RecordsNum, Data) ->
    ?debug([?MODULE, apply_template, {template_id, TemplateID}, {records_num, RecordsNum}]),
    case lists:keyfind(TemplateID, #template_rec.id, Templates) of
        false -> {error, {no_template, TemplateID, RecordsNum, Data}};
        %% TODO:
        #template_rec{id = TemplateID, fields = TemplateFields} ->
            {ok, _DetemplatedData} = apply_template_field_recs(TemplateFields, Data)
    end.

apply_template_field_recs(TemplateFields, Data) ->
    apply_template_field_recs(TemplateFields, Data, []).

apply_template_field_recs([], _, Acc) -> {ok, Acc};
apply_template_field_recs(
  [#template_field_rec{length = Length, type = Type} | TemplateFields], Data, Acc
 ) ->
    <<DataField:Length/binary, Rest/binary>> = Data,
    FormattedData = format_data_field(Type, Length, DataField),
    apply_template_field_recs(TemplateFields, Rest, [FormattedData | Acc]).

format_data_field(?IN_BYTES, Length, DataField) ->
    {?IN_BYTES, Length, binary:decode_unsigned(DataField, big)};
format_data_field(?L4_SRC_PORT, Length, DataField) ->
    {?L4_SRC_PORT, Length, DataField};
format_data_field(Type, Length, DataField) ->
    {Type, Length, DataField}.

apply_templates(_Templates, [], Processed, Unprocessed) ->
    {Processed, Unprocessed};
apply_templates(
  Templates, [{data, TemplateID, RecordsNum, Data} | DataSet],
  Processed0, Unprocessed0
 ) ->
    case apply_template(Templates, TemplateID, RecordsNum, Data) of
        {error, {no_template, _, _, _}} ->
            Unprocessed1 = [{data, TemplateID, RecordsNum, Data} | Unprocessed0],
            apply_templates(Templates, DataSet, Processed0, Unprocessed1);
        {error, Error} ->
            ?error([?MODULE, apply_templates, {error, Error}]),
            apply_templates(Templates, DataSet, Processed0, Unprocessed0);
        {ok, ProcessedData} ->
            Processed1 = [ProcessedData | Processed0],
            apply_templates(Templates, DataSet, Processed1, Unprocessed0)
    end.


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

decode_single_template(0, Rest, Acc) -> {lists:reverse(Acc), Rest};
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
