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
         t_no_arg/1,
         t_duplicated_args/1,
         t_unknown_arg/1,
         t_arg_types/1,
         t_missing_mandatory_arg/1,
         t_constraint_pass/1,
         t_constraint_fail/1
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
    [{cmd_spec, new_cmd_spec()} | Config].

end_per_testcase(_TestCaseName, _Config) ->
    ok.

new_cmd_spec() ->
    Cmd1Args = [{atom_arg, {atom,fun(Value) -> lists:member(Value, [a,b,c]) end}, true},
                {boolean_arg, boolean, false},
                {flag_arg, flag, true},
                {string_arg, string, false},
                {integer_arg, integer, true}
               ],
    SubCmd1 = new_sub_cmd_spec(sub_cmd1, Cmd1Args),

    Cmd2Args = [{boolean_arg, boolean, false},
                {integer_arg, {integer, fun(Value) -> Value >= 10 end}, true}
               ],
    SubCmd2 = new_sub_cmd_spec(sub_cmd2, Cmd2Args),

    SubCmd3 = new_sub_cmd_spec(sub_cmd3, _NoArgs=[]),

    argonomic:add_sub_cmd(argonomic:new_cmd(), [SubCmd1, SubCmd2, SubCmd3]).

new_sub_cmd_spec(SubCmdName, ArgSpecList) ->
    lists:foldl(fun({ArgName, Type, IsMandatory}, SubCmd) ->
                    Arg = argonomic:new_arg(ArgName, Type, IsMandatory),
                    argonomic:add_arg(SubCmd, Arg)
                end,
                _AccIn=argonomic:new_sub_cmd(SubCmdName),
                ArgSpecList
               ).

%%% ============================================================================
%%% Test Cases
%%% ============================================================================
%%% TODO 
%%% Print description
%%------------------------------------------------------------------------------
t_unknown_sub_cmd(Config) ->
    %% Arrange
    CmdSpec = proplists:get_value(cmd_spec, Config),

    %% Act
    UnknownSubCmd = "some_unknown_sub_command",
    Args = [UnknownSubCmd,
            "-atom_arg", "some_atom"
           ],
    Result = (catch argonomic:parse(CmdSpec, Args)),
    
    %% Assert
    ?assertEqual({'EXIT', {unknown_sub_command, list_to_atom(UnknownSubCmd)}},
                 Result
                ).

%%------------------------------------------------------------------------------
t_no_arg(Config) ->
    %% Arrange
    CmdSpec = proplists:get_value(cmd_spec, Config),

    %% Act
    Args = ["sub_cmd3"],
    Result = argonomic:parse(CmdSpec, Args),

    %% Assert
    ?assertEqual({sub_cmd3, []}, Result).

%%------------------------------------------------------------------------------
t_duplicated_args(Config) ->
    %% Arrange
    CmdSpec = proplists:get_value(cmd_spec, Config),

    %% Act
    Args = ["sub_cmd1",
            "-boolean_arg", "false",
            "-boolean_arg", "true"
           ],
    Result = (catch argonomic:parse(CmdSpec, Args)),

    %% Assert
    %% The parsed args will be removed from spec during parsing,
    %% so the second time the same arg is parsed it will be unknown.
    ?assertEqual({'EXIT', {unknown_arg, boolean_arg}}, Result).

%%------------------------------------------------------------------------------
t_unknown_arg(Config) ->
    %% Arrange
    CmdSpec = proplists:get_value(cmd_spec, Config),

    %% Act
    %% -cmd2_arg is unknown to SubCmd1.
    Args = ["sub_cmd2",
            "-atom_arg", "b"
           ],
    Result = (catch argonomic:parse(CmdSpec, Args)),

    %% Assert
    ?assertEqual({'EXIT', {unknown_arg, atom_arg}}, Result).

%%------------------------------------------------------------------------------
t_arg_types(Config) ->
    %% Arrange
    CmdSpec = proplists:get_value(cmd_spec, Config),

    %% Act
    Args = ["sub_cmd1",
            "-string_arg", "hello world",
            "-boolean_arg", "true",
            "-atom_arg", "c",
            "-flag_arg",
            "-integer_arg", "1234"
           ],
    Result = argonomic:parse(CmdSpec, Args),

    %% Assert
    ?assertEqual({sub_cmd1, [{string_arg, "hello world"},
                             {boolean_arg, true},
                             {atom_arg, c},
                             flag_arg,
                             {integer_arg, 1234}
                            ]},
                 Result
                ).

%%------------------------------------------------------------------------------
t_missing_mandatory_arg(Config) ->
    %% Arrange
    CmdSpec = proplists:get_value(cmd_spec, Config),

    %% Act
    Args = ["sub_cmd2",
            "-boolean_arg", "true"
           ],
    Result = (catch argonomic:parse(CmdSpec, Args)),

    %% Assert
    ?assertEqual({'EXIT', {missing_mandatory_arg, integer_arg}}, Result).

%%------------------------------------------------------------------------------
t_constraint_pass(Config) ->
    %% Arrange
    CmdSpec = proplists:get_value(cmd_spec, Config),

    %% Act
    Args = ["sub_cmd2",
            "-integer_arg", "20"
           ],
    Result = argonomic:parse(CmdSpec, Args),

    %% Assert
    ?assertEqual({sub_cmd2, [{integer_arg, 20}]}, Result).

%%------------------------------------------------------------------------------
t_constraint_fail(Config) ->
    %% Arrange
    CmdSpec = proplists:get_value(cmd_spec, Config),

    %% Act
    Args = ["sub_cmd2",
            "-integer_arg", "5"
           ],
    Result = (catch argonomic:parse(CmdSpec, Args)),

    %% Assert
    ?assertEqual({'EXIT',{failed_constraint_check,integer_arg}}, Result).

