-module(argonomic).

%% -----------------------------------------------------------------------------
%% Exports
%% -----------------------------------------------------------------------------
-export([
         new_cmd/0,
         new_sub_cmd/2,
         add_sub_cmd/2,
         new_arg/5,
         add_arg/2,

         parse/2,

         print_help_msg/1,
         get_help_msg/1
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
-spec new_sub_cmd(sub_command_name(), Description::string()) -> #sub_command{}.
%%------------------------------------------------------------------------------
new_sub_cmd(Name, Description) ->
    #sub_command{name=Name, args=maps:new(), description=Description}.

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
-spec new_arg(Name, Type, IsList, Presence, Description) -> #arg{} when
      Name        :: arg_name(),
      Type        :: arg_type(),
      IsList      :: boolean(),
      Presence    :: presence(),
      Description :: string().
%%------------------------------------------------------------------------------
new_arg(Name, Type, IsList, Presence, Description) ->
    #arg{name = Name,
         type = Type,
         is_list = IsList,
         presence = Presence,
         description = Description
        }.

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

%%------------------------------------------------------------------------------
-spec print_help_msg(#sub_command{} | command()) -> any().
%%------------------------------------------------------------------------------
print_help_msg(Spec) ->
    io:format(get_help_msg(Spec)).

%%------------------------------------------------------------------------------
-spec get_help_msg(#sub_command{} | command()) -> string().
%%------------------------------------------------------------------------------
get_help_msg(#sub_command{args=Args, description=Description}) ->
    io_lib:format("~s~n~n~s", [Description, get_help_msg(Args)]);
get_help_msg(CmdSpec) ->
    SubCmds = maps:values(CmdSpec),
    lists:flatmap(fun format_spec/1, SubCmds).

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
parse_value(#arg{name=Name, is_list=IsList} = ArgSpec, Rest) ->
    {ParsedValue, NewRest} =
        case IsList of
            true ->
                parse_list_value(ArgSpec, Rest, _Parsed=[]);
            false ->
                [Next | Remaining] = Rest,
                {parse_single_value(ArgSpec, Next), Remaining}
        end,
    {{Name, ParsedValue}, NewRest}.

parse_list_value(_ArgSpec, [[$-|_NextArgName] | _] = Rest, ParsedValues) ->
    {lists:reverse(ParsedValues), Rest};
parse_list_value(ArgSpec, [Str | Rest], ParsedValues) ->
    Value = parse_single_value(ArgSpec, Str),
    parse_list_value(ArgSpec, Rest, [Value|ParsedValues]).

parse_single_value(#arg{name=Name, type=Type}, Str) ->
    case Type of
        {PrimitiveType, Constraint} ->
            Value = parse_primitive_value(PrimitiveType, Str),
            case Constraint(Value) of
                true  -> Value;
                false -> abort(failed_constraint_check, Name)
            end,
            Value;
        PrimitiveType ->
            parse_primitive_value(PrimitiveType, Str)
    end.

parse_primitive_value(PrimitiveType, Str) ->
    case PrimitiveType of
        boolean -> list_to_atom(Str);
        string  -> Str;
        atom    -> list_to_atom(Str);
        integer -> list_to_integer(Str)
    end.

%%------------------------------------------------------------------------------
-spec abort(ErrorMsg::term(), Details::term()) -> no_return().
%%------------------------------------------------------------------------------
abort(ErrorMsg, Details) ->
    io:format("Error - ~p: ~p.~n", [ErrorMsg, Details]),
    exit({ErrorMsg, Details}).

format_spec(#sub_command{name=Name, description=Description}) ->
    io_lib:format("~-20.. s ~s~n", [atom_to_list(Name), Description]);
format_spec(#arg{} = ArgSpec) ->
    #arg{name = Name,
         type = Type,
         presence = Presence,
         description = Description
        } = ArgSpec,
    ArgStr = "-" ++ atom_to_list(Name),
    NewArgStr = case Presence of
                    mandatory -> ArgStr;
                    optional  -> "[" ++ ArgStr ++ "]"
                end,
            
    PrimitiveType = get_primitive_type(Type),
    io_lib:format("~-20.. s ~-10.. s ~s~n", [NewArgStr, PrimitiveType, Description]).

get_primitive_type({PrimitiveType, _Constraint}) -> PrimitiveType;
get_primitive_type(PrimitiveType)                -> PrimitiveType.

