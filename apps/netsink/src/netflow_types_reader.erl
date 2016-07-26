%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(netflow_types_reader).

%% API

-export([types_bind/1, user_types/1]).

-include_lib("netsink/include/netflow.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec types_bind(Conf :: string()) -> term().
types_bind(Conf) ->
    {ok, _Terms} = file:consult(Conf).

-spec user_types(Conf :: string()) -> ok.
user_types(Conf) ->
    Exprs = user_types_config(Conf),
    ExprsParsed = lists:map(fun user_types_parse/1, Exprs),
    FuncAST = user_types_eval(ExprsParsed),
    ok = user_types_compile_and_load(FuncAST).

%%%===================================================================
%%% Internal functions
%%%===================================================================

user_types_compile_and_load(UserTypesFuncAST) ->
    Forms =
        [{attribute, 0, file, {"netflow_user_types", 1}},
         {attribute, 0, module, netflow_user_types},
         {attribute, 0, export, [{user_type, 3}]},
         UserTypesFuncAST
        ],
    ok = parse_trans_mod:compile_and_load_forms(Forms).

user_types_eval(ConfigExprs) ->
    FClauses =
        lists:flatten(
          [[
            begin
                Vars = erl_syntax:clause_patterns(Clause),
                ClauseGuard = erl_syntax:clause_guard(Clause),
                ClauseBody = erl_syntax:clause_body(Clause),
                erl_syntax:clause(
                  [erl_syntax:integer(ID) | Vars], ClauseGuard, ClauseBody)
            end || Clause <- Clauses ]
           || {ID, _Descr, Clauses} <- ConfigExprs]),
    Func = erl_syntax:function(erl_syntax:atom(user_type), FClauses),
    _FReady = erl_syntax:revert(Func).

user_types_parse(E) ->
    tuple = erl_syntax:type(E),
    3 = erl_syntax:tuple_size(E),
    [IDExpr, DescrExpr, FunExpr] = erl_syntax:tuple_elements(E),
    {value, ID, _} = erl_eval:expr(IDExpr, []),
    {value, Descr, _} = erl_eval:expr(DescrExpr, []),
    fun_expr = erl_syntax:type(FunExpr),
    2 = erl_syntax:fun_expr_arity(FunExpr),
    Clauses = erl_syntax:fun_expr_clauses(FunExpr),
    {ID, Descr, Clauses}.

user_types_config(Config) ->
    {ok, Bin} = file:read_file(Config),
    {ok, ErlTokens, _} = erl_scan:string(binary_to_list(Bin)),
    TopExprs =
        case erl_parse:parse_exprs(ErlTokens) of
            {error, _} -> throw(config_error);
            {ok, [Exprs]} -> Exprs
        end,
    case erl_syntax:type(TopExprs) of
        list -> erl_syntax:list_elements(TopExprs);
        _ -> throw(config_error)
    end.
