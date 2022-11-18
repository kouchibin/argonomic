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
         t_unknown_sub_cmd/1,
         t_unknown_arg/1,
         t_arg_types/1
        ]).


%%% ============================================================================
%%% Include files
%%% ============================================================================
-include_lib("stdlib/include/assert.hrl").

%%% ============================================================================
%%% Defines
%%% ============================================================================

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
%%% Print description
%%% Constraint
t_unknown_sub_cmd(_Config) ->
    %% Arrange
    SubCmdName = some_sub_cmd,
    SubCmd = new_sub_cmd_spec(SubCmdName),
    CmdSpec = argonomic:add_sub_cmd(argonomic:new_cmd(), SubCmd),

    %% Act
    Args = ["some_unknown_sub_command",
            "-atom_arg", "some_atom"
           ],
    Result = (catch argonomic:parse(CmdSpec, Args)),
    
    %% Assert
    ?assertEqual({'EXIT', unknown_sub_command}, Result).

t_unknown_arg(_Config) ->
    %% Arrange
    SubCmdName1 = some_sub_cmd1,
    SubCmd1 = new_sub_cmd_spec(SubCmdName1),

    SubCmdName2 = some_sub_cmd2,
    Arg = argonomic:new_arg(_Name=cmd2_arg, _Type=boolean, _IsMandatory=true),
    SubCmd2 = argonomic:add_arg(argonomic:new_sub_cmd(SubCmdName2), Arg),

    CmdSpec = argonomic:add_sub_cmd(argonomic:new_cmd(),
                                    [SubCmd1, SubCmd2]
                                   ),

    %% Act
    %% -cmd2_arg is unknown to SubCmd1.
    Args = ["some_sub_cmd1",
            "-cmd2_arg", "false"
           ],
    Result = (catch argonomic:parse(CmdSpec, Args)),

    %% Assert
    ?assertEqual({'EXIT', unknown_arg}, Result).

new_sub_cmd_spec(SubCmdName) ->
    lists:foldl(fun({ArgName, Type}, SubCmd) ->
                    Arg = argonomic:new_arg(ArgName, Type, _IsMandatory=true),
                    argonomic:add_arg(SubCmd, Arg)
                end,
                _AccIn=argonomic:new_sub_cmd(SubCmdName),
                [{atom_arg, atom},
                 {boolean_arg, boolean},
                 {flag_arg, flag},
                 {string_arg, string},
                 {integer_arg, integer}
                ]
               ).

t_arg_types(_Config) ->
    %% Arrange
    SubCmdName = some_sub_cmd,
    SubCmd = new_sub_cmd_spec(SubCmdName),
    CmdSpec = argonomic:add_sub_cmd(argonomic:new_cmd(), SubCmd),

    %% Act
    Args = ["some_sub_cmd",
            "-string_arg", "hello world",
            "-boolean_arg", "true",
            "-atom_arg", "some_atom",
            "-flag_arg",
            "-integer_arg", "1234"
           ],
    Result = argonomic:parse(CmdSpec, Args),

    %% Assert
    ?assertEqual({SubCmdName, [{string_arg, "hello world"},
                               {boolean_arg, true},
                               {atom_arg, some_atom},
                               flag_arg,
                               {integer_arg, 1234}
                              ]},
                 Result
                ).


