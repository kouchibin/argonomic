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
         t_arg_types/1
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
t_arg_types(_Config) ->
    %% Arrange
    SubCmd = lists:foldl(fun({ArgName, Type}, SubCmd) ->
                             Arg = argonomic:new_arg(ArgName, Type, _IsMandatory=true),
                             argonomic:add_arg(SubCmd, Arg)
                         end,
                         _AccIn=argonomic:new_sub_cmd(sub_cmd1),
                         [{atom_arg, atom},
                          {boolean_arg, boolean},
                          {flag_arg, flag},
                          {string_arg, string},
                          {integer_arg, integer}
                         ]
                        ),
    CmdSpec = argonomic:add_sub_cmd(argonomic:new_cmd(), SubCmd),

    %% Act
    Args = ["sub_cmd1",
            "-string_arg", "hello world",
            "-boolean_arg", "true",
            "-atom_arg", "some_atom",
            "-flag_arg",
            "-integer_arg", "1234"
           ],
    Result = argonomic:parse(CmdSpec, Args),

    %% Assert
    ?assertEqual({sub_cmd1, [{string_arg, "hello world"},
                             {boolean_arg, true},
                             {atom_arg, some_atom},
                             flag_arg,
                             {integer_arg, 1234}
                            ]},
                 Result
                ).


