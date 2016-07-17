%%%-------------------------------------------------------------------
%%% @author Stanislav M. Ivankin <lessgrep@gmail.com>
%%% @copyright (C) 2016, Stanislav M. Ivankin
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2016 by Stanislav M. Ivankin <lessgrep@gmail.com>
%%%-------------------------------------------------------------------
-module(test).

-define(CONFIG_1, "etc/netflow/id_binds.cfg").
-define(CONFIG_2, "etc/netflow/user_types.cfg").

%% API
-export([config1/0, config2/0, user_types_generate/0]).

%%%===================================================================
%%% API
%%%===================================================================

config1() ->
    {ok, Terms} = file:consult(?CONFIG_1),
    Terms.

config2() ->
    user_types_generate().

config(Config) ->
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

user_types_generate() ->
    Exprs = config(?CONFIG_2),
    ExprsParsed = lists:map(fun config2_entity_parse/1, Exprs),
    FuncAST = user_types_eval(ExprsParsed),
    ok = user_types_compile_and_load(FuncAST).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

config2_entity_parse(E) ->
    tuple = erl_syntax:type(E),
    3 = erl_syntax:tuple_size(E),
    [IDExpr, DescrExpr, FunExpr] = erl_syntax:tuple_elements(E),
    {value, ID, _} = erl_eval:expr(IDExpr, []),
    {value, Descr, _} = erl_eval:expr(DescrExpr, []),
    fun_expr = erl_syntax:type(FunExpr),
    2 = erl_syntax:fun_expr_arity(FunExpr),
    Clauses = erl_syntax:fun_expr_clauses(FunExpr),
    {ID, Descr, Clauses}.

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

user_types_compile_and_load(UserTypesFuncAST) ->
    Forms =
        [{attribute, 0, file, {"netflow_user_types", 1}},
         {attribute, 0, module, netflow_user_types},
         {attribute, 0, export, [{user_type, 3}]},
         UserTypesFuncAST
        ],
    parse_trans_mod:compile_and_load_forms(Forms).
