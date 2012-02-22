-module(ucol_data_unidata).
-export([generate/0, compile/1, load/0]).

-record(acc, {
    ducet=ucol_array:new(),
    var_ducet=ucol_array:new(),
    ccc,
    decomp=array:new()
}).

generate() ->
    DucetTable = (ux_unidata_filelist:get_source(allkeys, ducet))(get_table),
    Acc1 = #acc{},
    Acc2 = ets:foldl(fun ducet_handler/2, Acc1, DucetTable),

    %% Get table id
    DecompTable = (ux_unidata_filelist:get_source(unidata, decomp))(get_table),
    %% Convert to list
    DecompList = ets:tab2list(DecompTable),
    %% Full Decompose
    NewDecompList = lists:map(fun full_decompose_row/1, DecompList),
%   Acc3 = lists:foldl(fun decomp_handler/2, Acc2, NewDecompList),
    Acc3 = Acc2,

    CccTable = (ux_unidata_filelist:get_source(unidata, ccc))(get_table),
    CccList = ets:tab2list(CccTable),
    NewCccList = lists:foldl(fun decomp_ccc_handler/2, CccList, NewDecompList),
    CccMap = ucol_map:from_list(NewCccList),
    Acc4 = Acc3#acc{ccc=CccMap},

    %% Collect Decomp Data
    lists:foldl(fun collect_decomp_handler/2, Acc4, NewDecompList).


%% If [Char] is in ducet;
%%      and to_nfd([Char]) is not in ducet,
%%      then delete [Char] from ducet.
array_set([_]=Key1, Weight, Ducet) ->
    Res = case ux_string:to_nfd(Key1) of
        [_Char]=Key2 -> 
            case ux_unidata:ducet(Key2) of
                other -> 
                    Ducet;
%                   Ducet1 = ucol_array:set(Key1, Weight, Ducet),
%                            ucol_array:set(Key2, Weight, Ducet1);
                _ -> default
            end;
        _ -> default
    end,
    case Res of
        default -> ucol_array:set(Key1, Weight, Ducet);
        X -> X
    end;

array_set(Key, Weight, Ducet) ->
    ucol_array:set(Key, Weight, Ducet).


%% For ducet table. Change ducet, var_ducet.
ducet_handler({Key, Bin}, Acc=#acc{ducet=Ducet, var_ducet=VarDucet})
    when is_binary(Bin) -> 
    %% Bin is a binary weight
    {NonVarWeight, VarWeight} = ucol_weights:decode(Bin),
    NewDucet = case ucol_weights:is_empty(NonVarWeight) of
        false -> array_set(Key, NonVarWeight, Ducet);
        true -> array_set(Key, ucol_weights:empty(NonVarWeight), Ducet) end,
    NewVarDucet = case ucol_weights:is_empty(VarWeight) of
        false -> array_set(Key, VarWeight, VarDucet);
        true -> array_set(Key, ucol_weights:empty(VarWeight), VarDucet) end,
    Acc#acc{ducet=NewDucet, var_ducet=NewVarDucet};

ducet_handler(Info, Acc) -> 
    io:format("Info: ~w~n", [Info]), Acc.


%% For decomp table. Change ducet, var_ducet.
%decomp_handler({Char, List}, Acc=#acc{ducet=Ducet, var_ducet=VarDucet}) -> 
%    case ux_unidata:is_compat(Char) of
%    false ->
%        try 
%            Key = [Char],
%            Bin = ux_uca_decomp:compute(Char, List),
%            {NonVarWeight, VarWeight} = ucol_weights:decode(Bin),
%            NewDucet    = array_set(Key, NonVarWeight, Ducet),
%            NewVarDucet = array_set(Key, VarWeight, VarDucet),
%            Acc#acc{ducet=NewDucet, var_ducet=NewVarDucet}
%        catch throw:{bad_char, BadChar} -> 
%            io:format("Skip a decomp value (~w): ~w ~w~n", 
%                [BadChar, Char, List]), 
%            Acc
%        end;
%    % Char is cannonical. Cannot be decompose.
%    true -> Acc 
%    end.

%% For decomp table. Change ccc.
%decomp_ccc_handler({Char, [ReplaceChar]}, CccList) ->
%    Ccc1 = ux_unidata:ccc(Char),
%    Ccc2 = ux_unidata:ccc(ReplaceChar),
%    if Ccc1 =:= Ccc2 -> CccList;
%        true -> 
%            %% Replace old value
%            [{Char, Ccc2} | proplists:delete(Char, CccList)]
%    end;

decomp_ccc_handler({Char, _ReplaceStr}, CccList) ->
    %% Lets make ccc=DECOMP_CLASS for composed values. 
    %% It is free.
    %% Chars with ccc=DECOMP_CLASS MUST be decomposed manually.
    %%
    %% http://unicode.org/reports/tr44/tr44-4.html#Canonical_Combining_Class_Values
    SpecialCcc = ucol_string:decomp_class(),
    [{Char, SpecialCcc} | proplists:delete(Char, CccList)];

decomp_ccc_handler(_, CccList) -> CccList.


full_decompose_row({Char, _ReplaceStr}) ->
    {Char, ux_string:to_nfd([Char])}.


%collect_decomp_handler({_Char, [_ReplaceChar]}, Acc) -> Acc;
collect_decomp_handler({Char, ReplaceStr}, Acc=#acc{decomp=DecompArr}) ->
    Elem = [{X, ux_unidata:ccc(X)} || X <- ReplaceStr],
    NewDecompArr = array:set(Char, Elem, DecompArr),
    Acc#acc{decomp=NewDecompArr}.


compile(#acc{ducet=Ducet, var_ducet=VarDucet, ccc=Ccc, decomp=Decomp}) ->
    {ok, MTs, _} = erl_scan:string("-module(ucol_unidata)."),
    {ok, ETs, _} = erl_scan:string("-compile([export_all])."),

    {ok,MF} = erl_parse:parse_form(MTs),
    {ok,EF} = erl_parse:parse_form(ETs),

    AbsDucet = erl_parse:abstract(Ducet),
    AbsVarDucet = erl_parse:abstract(VarDucet),
    AbsCcc = erl_parse:abstract(Ccc),
    AbsDecomp = erl_parse:abstract(Decomp),

    DucetF    = function(ducet, AbsDucet),
    VarDucetF = function(var_ducet, AbsVarDucet),
    CccF      = function(ccc, AbsCcc),
    DecompF   = function(decomp, AbsDecomp),

    compile:forms([MF, EF, DucetF, VarDucetF, CccF, DecompF]).


%% Returns forms
function(Name, Result) ->
    erl_syntax:revert(
        % AST
        erl_syntax:function(erl_syntax:atom(Name),
           [erl_syntax:clause([], none, [Result])])).

load() ->
    {ok, Name, Bin} = ucol_data:compile(ucol_data:generate()),
    code:load_binary(Name, "ucol_unidata", Bin).
