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
-record(arg, {name            :: arg_name(),
              type            :: arg_type(),
              is_list = false :: boolean(),
              presence        :: presence(),
              description     :: string()
             }).

-record(sub_command, {name        :: sub_command_name(),
                      args        :: arg_specs(),
                      description :: string()
                     }).

-type arg_name()         :: atom().
-type sub_command_name() :: atom().
-type presence()         :: mandatory | optional.

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
-spec new_arg(arg_name(), arg_type(), presence()) -> #arg{}.
%%------------------------------------------------------------------------------
new_arg(Name, Type, Presence) ->
    #arg{name=Name, type=Type, presence=Presence}.

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
-spec parse_args(#sub_command{}, Args::[string()]) -> proplists:proplist() | no_return().
%%------------------------------------------------------------------------------
parse_args(SubCmdSpec, ArgStrs) ->
    parse_args_help(SubCmdSpec, ArgStrs, _ParsedResult=[]).

%%------------------------------------------------------------------------------
parse_args_help(#sub_command{args=ArgSpecs}, _ArgStrs=[], ParsedResult) ->
    check_if_missing_mandatory_arg(ArgSpecs),
    lists:reverse(ParsedResult);
parse_args_help(SubCmdSpec, _ArgStrs=[[$-|ArgStr] | Rest], ParsedResult) ->
    ArgName = list_to_atom(ArgStr),
    ArgSpecs = SubCmdSpec#sub_command.args,
    ArgSpec = get_mandatory_val(ArgName, ArgSpecs, unknown_arg),
    {ParsedArg, NewRest} = parse_value(ArgSpec, Rest),
    RemainingArgSpecs = maps:remove(ArgName, ArgSpecs),
    parse_args_help(SubCmdSpec#sub_command{args=RemainingArgSpecs},
                    NewRest,
                    [ParsedArg | ParsedResult]
                   ).

%%------------------------------------------------------------------------------
check_if_missing_mandatory_arg(ArgSpecs) ->
    lists:foreach(
      fun({ArgName, #arg{presence=Presence}}) ->
               case Presence of
                   mandatory -> abort(missing_mandatory_arg, ArgName);
                   optional  -> ok
               end
       end,
       maps:to_list(ArgSpecs)
      ).

%%------------------------------------------------------------------------------
-spec get_mandatory_val(Key, Map, ErrorMsg) -> Value | no_return() when
      Key      :: term(),
      Map      :: #{Key => Value},
      ErrorMsg :: term(),
      Value    :: term().
%%------------------------------------------------------------------------------
get_mandatory_val(Key, Map, ErrorMsg) ->
    case maps:get(Key, Map, ErrorMsg) of
        ErrorMsg -> abort(ErrorMsg, Key);
        Value    -> Value
    end.

%%------------------------------------------------------------------------------
-spec parse_value(#arg{}, Args::[string()]) -> {proplists:property(), Rest::[string()]}.
%%------------------------------------------------------------------------------
parse_value(#arg{name=FlagName, type=flag}, Rest) ->
    {FlagName, Rest};
parse_value(#arg{name=Name, type=Type}, [Next | Rest]) ->
    ParsedValue = case Type of
                      {PrimitiveType, Constraint} ->
                          Value = parse_primitive_value(PrimitiveType, Next),
                          case Constraint(Value) of
                              true ->
                                  Value;
                              false ->
                                  abort(failed_constraint_check, Name)
                          end;
                      PrimitiveType ->
                          parse_primitive_value(PrimitiveType, Next)
                  end,
    {{Name, ParsedValue}, Rest}.

parse_primitive_value(PrimitiveType, Str) ->
    case PrimitiveType of
        boolean -> list_to_atom(Str);
        string  -> Str;
        atom    -> list_to_atom(Str);
        integer -> list_to_integer(Str)
    end.

abort(ErrorMsg, Details) ->
    io:format("Error - ~p: ~p.~n", [ErrorMsg, Details]),
    exit({ErrorMsg, Details}).

