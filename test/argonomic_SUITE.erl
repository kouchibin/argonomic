-module(argonomic_SUITE).

%%% ============================================================================
%%% Exports
%%% ============================================================================

%% CT framework exports
-export([
         all/0,
         groups/0,
         suite/0,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

%% Test cases
-export([
         t_parse/1
        ]).


%%% ============================================================================
%%% Include files
%%% ============================================================================
% -include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%% ============================================================================
%%% Defines
%%% ============================================================================

% -define(arg_spec, [{name, }])
%%% ============================================================================
%%% Common Test Functions
%%% ============================================================================

all() ->
    [{group, normal}].

groups() ->
    [{normal, [], all_test_cases()}].

suite() ->
    [{timetrap, {seconds, 10}}].

all_test_cases() ->
    lists:filtermap(fun({TestCase, _Arity}) ->
                        case lists:prefix("t_", atom_to_list(TestCase)) of
                            true  -> {true, TestCase};
                            false -> false
                        end
                    end,
                    module_info(exports)
                ).

init_per_testcase(_TestCaseName, Config) ->
    Config.

end_per_testcase(_TestCaseName, _Config) ->
    ok.

%%% ============================================================================
%%% Test Cases
%%% ============================================================================
%%% TODO 
%%% unknown subcommand
%%% duplicated args
%%% subcommand with no args
%%% mandatory arg missing
%%% Print help message
%%%
t_parse(_Config) ->
    Arg1 = argonomic:new_arg(_ArgName=arg1, _Type=boolean, _IsMandatory=true),
    SubCmd = argonomic:new_sub_cmd(_SubCmdName = sub_cmd1),
    NewSubCmd = argonomic:add_arg(SubCmd, Arg1),

    Cmd = argonomic:new_cmd(),
    NewCmd = argonomic:add_sub_cmd(Cmd, NewSubCmd),

    Args = ["sub_cmd1", "-arg1", "true"],
    Result = argonomic:parse(NewCmd, Args),
    ?assertEqual({sub_cmd1, [{arg1, true}]}, Result).


