-module(argonomic).

-type command_spec() :: [sub_command_spec()].
-type sub_command_spec() :: {SubCmdName::atom(), [arg_spec()]}.
-type arg_spec() :: {Name::atom(), Type::value_type(), Mandatory::boolean(), Constraint::fun((Value::term())-> boolean())}.
-type value_type() :: atom | boolean | string | integer | flag.

-export([parse/2]).

%%------------------------------------------------------------------------------
-spec parse(command_spec(), Args::[string()]) -> proplists:proplist().
%%------------------------------------------------------------------------------
parse(CmdSpec, [SubCmdStr|ArgStrs]) ->
    SubCmd = list_to_atom(SubCmdStr),
    ArgSpecs = proplists:get_value(SubCmd, CmdSpec),
    ParsedArgs = parse_args(ArgSpecs, ArgStrs),
    {SubCmd, ParsedArgs}.

parse_args(ArgSpecs, ArgStrs) ->
    parse_args_help(ArgSpecs, ArgStrs, _ParsedResult=[]).

parse_args_help(_ArgSpecs, _ArgStrs=[], ParsedResult) ->
    lists:reverse(ParsedResult);
parse_args_help(ArgSpecs, _ArgStrs=[[$-|ArgStr], Next | Rest], ParsedResult) ->
    {ArgName, _Type, _Mandatory, _Constraint} = lists:keyfind(list_to_atom(ArgStr), 1, ArgSpecs),
    Value = list_to_atom(Next),
    NewParsedResult = [{ArgName, Value} | ParsedResult],
    parse_args_help(ArgSpecs, Rest, NewParsedResult).




    



