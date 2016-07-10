%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 10 Jul 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(netflow_types_compiler).

-export([start/1, init/2]).

-export([read_config/1, compile_netflow_types/1]).

-include_lib("netsink/include/netflow.hrl").

%% API

start(Conf) ->
    spawn(netflow_types_compiler, init, [self(), Conf]).

init(From, Conf) ->
    loop(From, Conf).

%% Internal

loop(From, Conf) ->
    receive
        _ ->
            loop(From, Conf)
    end.

read_config(ConfFile) ->
    {ok, Bin} = file:read_file(ConfFile),
    Str = binary_to_list(Bin),
    {ok, Tokens, _} = erl_scan:string(Str),
    TopExprs =
        case erl_parse:parse_exprs(Tokens) of
            {error, {Line, erl_parse, ErrorInfo}} ->
                error({conf_parser, {Line, ErrorInfo}});
            {ok, [Exprs]} -> Exprs
        end,
    ExprElements =
        case erl_syntax:type(TopExprs) of
            list -> erl_syntax:list_elements(TopExprs);
            _ -> error({conf_parser, unknown_format})
        end,
    netflow_formatter_recs(ExprElements).

netflow_formatter_recs([]) -> [];
netflow_formatter_recs([E|Elements]) ->
    case erl_syntax:type(E) == tuple andalso
        erl_syntax:tuple_size(E) == 2 of
        true ->
            [ConfType, ConfValues] = erl_syntax:tuple_elements(E),
            case erl_syntax:type(ConfType) == atom andalso
                erl_syntax:atom_value(ConfType) == types of
                true ->
                    ConfListValues = erl_syntax:list_elements(ConfValues),
                    lists:map(fun formatter_conf_value_to_rec/1, ConfListValues);
                false -> netflow_formatter_recs(Elements)
            end;
        false -> netflow_formatter_recs(Elements)
    end.

formatter_conf_value_to_rec(Value) ->
    tuple = erl_syntax:type(Value),
    3 = erl_syntax:tuple_size(Value),
    [ExprTypeName, ExprTypeID, ExprFormatter] = erl_syntax:tuple_elements(Value),
    {value, TypeName, _} = erl_eval:expr(ExprTypeName, []),
    {value, TypeID, _}  = erl_eval:expr(ExprTypeID, []),
    fun_expr = erl_syntax:type(ExprFormatter),
    1 = erl_syntax:fun_expr_arity(ExprFormatter),
    [Clause] = erl_syntax:fun_expr_clauses(ExprFormatter),
    #netflow_type_formatter{name = TypeName, type_id = TypeID, formatter_clause = Clause}.


compile_netflow_types(Recs) ->
    Forms =
        [
         {attribute, 0, file, "netflow_defs", 1},
         {attribute, 0, module, netflow_defs},
         {attribute, 0, export, [{data_format, 1}]},
         netflow_data_format_ast_code(Recs)
        ],
    ok = compile_and_load(Forms).

netflow_data_format_ast_code(Recs) ->
    FuncClauses =
        lists:flatten(
          lists:map(
            fun (#netflow_type_formatter{name = _Name, type_id = _ID, formatter_clause = Clause}) ->
                    ClausePatterns = erl_syntax:clause_patterns(Clause),
                    ClauseGuard = erl_syntax:clause_guard(Clause),
                    ClauseBody = erl_syntax:clause_body(Clause),
                    %% erl_syntax:atom(Name), erl_syntax:integer(ID),
                    erl_syntax:clause([ClausePatterns], ClauseGuard, ClauseBody)
            end, Recs)
         ),
    Func = erl_syntax:function(
             erl_syntax:atom(data_format), FuncClauses
            ),
    erl_syntax:revert(Func).

compile_and_load(Forms) ->
    ok = parse_trans_mod:compile_and_load_forms(Forms).
