-module(argonomic).

%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------
-export([
         new_cmd/0,
         new_sub_cmd/1,
         add_sub_cmd/2,
         new_arg/3,
         add_arg/2,
         parse/2
        ]).

%% -----------------------------------------------------------------------------
%% Definitions
%% -----------------------------------------------------------------------------
-record(arg, {name         :: arg_name(),
              type         :: arg_type(),
              is_mandatory :: boolean(),
              description  :: string()
             }).

-record(sub_command, {name        :: sub_command_name(),
                      args        :: arg_specs(),
                      description :: string()
                     }).

-type arg_name()         :: atom().
-type sub_command_name() :: atom().

-type command() :: #{sub_command_name() => #sub_command{}}.

-type arg_specs() :: #{arg_name() => #arg{}}.

-type arg_type() :: value_type() |
                    {value_type(), Constraint::fun((Value::term())-> boolean())} |
                    flag.

-type value_type() :: atom | boolean | string | integer.

%% -----------------------------------------------------------------------------
%% Externally Exported Functions
%% -----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
-spec new_cmd() -> command().
%%------------------------------------------------------------------------------
new_cmd() ->
    maps:new().

%%------------------------------------------------------------------------------
-spec new_sub_cmd(sub_command_name()) -> #sub_command{}.
%%------------------------------------------------------------------------------
new_sub_cmd(Name) ->
    #sub_command{name=Name, args=maps:new()}.

%%------------------------------------------------------------------------------
-spec add_sub_cmd(command(), #sub_command{} | [#sub_command{}]) -> command().
%%------------------------------------------------------------------------------
add_sub_cmd(Cmd, _SubCmds=[]) ->
    Cmd;
add_sub_cmd(Cmd, [SubCmd | Rest]) ->
    NewCmd = add_sub_cmd(Cmd, SubCmd),
    add_sub_cmd(NewCmd, Rest);
add_sub_cmd(Cmd, #sub_command{name=Name} = SubCmd) ->
    maps:put(Name, SubCmd, Cmd).

%%------------------------------------------------------------------------------
-spec new_arg(arg_name(), arg_type(), IsMandatory::boolean()) -> #arg{}.
%%------------------------------------------------------------------------------
new_arg(Name, Type, IsMandatory) ->
    #arg{name=Name, type=Type, is_mandatory=IsMandatory}.

%%------------------------------------------------------------------------------
-spec add_arg(#sub_command{}, #arg{} | [#arg{}]) -> #sub_command{}.
%%------------------------------------------------------------------------------
add_arg(SubCmd, _Args=[]) ->
    SubCmd;
add_arg(SubCmd, [Arg | Rest]) ->
    NewSubCmd = add_arg(SubCmd, Arg),
    add_arg(NewSubCmd, Rest);
add_arg(#sub_command{args=Args} = SubCmd, #arg{name=ArgName} = Arg) ->
    SubCmd#sub_command{args=maps:put(ArgName, Arg, Args)}.

%%------------------------------------------------------------------------------
-spec parse(command(), Args::[string()]) -> {sub_command_name(), proplists:proplist()}.
%%------------------------------------------------------------------------------
parse(CmdSpec, [SubCmdStr|ArgStrs]) ->
    SubCmdName = list_to_atom(SubCmdStr),
    SubCmdSpec = get_mandatory_val(SubCmdName, CmdSpec, unknown_sub_command),
    ParsedArgs = parse_args(SubCmdSpec, ArgStrs),
    {SubCmdName, ParsedArgs}.

%% -----------------------------------------------------------------------------
%% Internal Functions
%% -----------------------------------------------------------------------------
 
%%------------------------------------------------------------------------------
-spec parse_args(#sub_command{}, Args::[string()]) -> proplists:proplist().
%%------------------------------------------------------------------------------
parse_args(SubCmdSpec, ArgStrs) ->
    parse_args_help(SubCmdSpec, ArgStrs, _ParsedResult=[]).

parse_args_help(_SubCmdSpec, _ArgStrs=[], ParsedResult) ->
    lists:reverse(ParsedResult);
parse_args_help(SubCmdSpec, _ArgStrs=[[$-|ArgStr] | Rest], ParsedResult) ->
    ArgName = list_to_atom(ArgStr),
    ArgSpec = get_mandatory_val(ArgName, SubCmdSpec#sub_command.args, unknown_arg),
    {ParsedArg, NewRest} = parse_value(ArgSpec, Rest),
    parse_args_help(SubCmdSpec, NewRest, [ParsedArg | ParsedResult]).

%%------------------------------------------------------------------------------
-spec get_mandatory_val(Key, Map, ErrorMsg) -> Value | no_return() when
      Key      :: term(),
      Map      :: #{Key => Value},
      ErrorMsg :: term(),
      Value    :: term().
%%------------------------------------------------------------------------------
get_mandatory_val(Key, Map, ErrorMsg) ->
    case maps:get(Key, Map, ErrorMsg) of
        ErrorMsg ->
            io:format("Error - ~p: ~p.~n", [ErrorMsg, Key]),
            exit(ErrorMsg);
        Value ->
            Value
    end.

%%------------------------------------------------------------------------------
-spec parse_value(#arg{}, Args::[string()]) -> {proplists:property(), Rest::[string()]}.
%%------------------------------------------------------------------------------
parse_value(#arg{name=FlagName, type=flag}, Rest) ->
    {FlagName, Rest};
parse_value(#arg{name=Name, type=Type}, [Next | Rest]) ->
    Value = case Type of
                boolean -> list_to_atom(Next);
                string  -> Next;
                atom    -> list_to_atom(Next);
                integer -> list_to_integer(Next)
            end,
    {{Name, Value}, Rest}.

    



