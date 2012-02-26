-module(ucol_data_testdata).
-export([generate/0, compile/1, load/0]).

-record(acc, {
    shifted
}).

generate() ->
    DecodedList = [unicode:characters_to_binary(X) 
        || X <- ux_uca_testdata:read_shifted()],
    Filter = fun(Y) -> is_binary(Y) end,
    {FilteredList, Errors} = lists:partition(Filter, DecodedList),
    [io:format("Test Case Decode Error: ~w~n", [E]) || E <- Errors],
    #acc{
        shifted=FilteredList
    }.
    



compile(#acc{shifted=Shifted}) ->
    {ok, MTs, _} = erl_scan:string("-module(ucol_testdata)."),
    {ok, ETs, _} = erl_scan:string("-compile([export_all])."),

    {ok,MF} = erl_parse:parse_form(MTs),
    {ok,EF} = erl_parse:parse_form(ETs),

    AbsShifted = erl_parse:abstract(Shifted),

    ShiftedF  = function(shifted, AbsShifted),

    compile:forms([MF, EF, ShiftedF]).


%% Returns forms
function(Name, Result) ->
    erl_syntax:revert(
        % AST
        erl_syntax:function(erl_syntax:atom(Name),
           [erl_syntax:clause([], none, [Result])])).

load() ->
    {ok, Name, Bin} = ucol_data:compile(ucol_data:generate()),
    code:load_binary(Name, "ucol_testdata", Bin).
