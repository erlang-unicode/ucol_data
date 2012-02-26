-module(ucol_data_unidata).
-export([generate/0, 
         compile/1, 
         load/0]).


-record(acc, {
    implicit=[],
    ducet=ucol_array:new(),
    var_ducet=ucol_array:new(),
    primary_ducet=ucol_array:new(),
    ccc,
    decomp=array:new()
}).

-define(INFO(Str), error_logger:info_msg("[" ++ ?MODULE_STRING ++ "] " ++ Str)).


generate() ->
    ?INFO("Start generation."),
    DucetTable = (ux_unidata_filelist:get_source(allkeys, ducet))(get_table),
    Acc1 = #acc{},
    Acc2 = ets:foldl(fun ducet_handler/2, Acc1, DucetTable),
    ?INFO("Ducet arrays were formed."),

    BlocksTable = (ux_unidata_filelist:get_source(blocks, block))(get_table),
    Acc3 = finish_blocks(ets:foldl(fun blocks_handler/2, Acc2, BlocksTable)),
    ?INFO("Implicit blocks were formed."),
    

    DecompTable = (ux_unidata_filelist:get_source(unidata, decomp))(get_table),
    DecompList = ets:tab2list(DecompTable),
    ?INFO("DecompTable was transformed into a list."),

    %% The table also contains NFKD decompositions (don't need them).
    %% OldDecomp can contain points which can also be decomposed.
    %% Full decomposition, filter standard (compat) compositions.
    NewDecompList = [
        {Point, ux_string:to_nfd([Point])}
            || {Point, _OldDecomp} <- DecompList, 
                not ux_unidata:is_compat(Point)],
    ?INFO("NFD elements were formed."),

    CccTable = (ux_unidata_filelist:get_source(unidata, ccc))(get_table),
    CccList = ets:tab2list(CccTable),
    ?INFO("ccc table was transformed into a list."),

    NewCccList = lists:foldl(fun decomp_ccc_handler/2, CccList, NewDecompList),
    ?INFO("Special classes for composed elements were installed."),

    CccMap = ucol_map:from_list(NewCccList),
    Acc4 = Acc3#acc{ccc=CccMap},
    ?INFO("Composition class index was formed."),

    Acc5 = lists:foldl(fun collect_decomp_handler/2, Acc4, NewDecompList),
    ?INFO("Decomposition table was fully decomposed."),
    fix(Acc5).


fix(Acc=#acc{
    ducet=D,
    var_ducet=VD,
    primary_ducet=PD,
    decomp=DC
}) -> 
    NewD = ucol_array:fix(D),
    ?INFO("Ducet array was fixed."),

    NewVD = ucol_array:fix(VD),
    ?INFO("Variable ducet array was fixed."),

    NewPD = ucol_array:fix(PD),
    ?INFO("Primary ducet array was fixed."),

    NewDC = array:fix(DC),
    ?INFO("Decomp array was fixed."),

    Acc#acc{
        ducet=NewD,
        var_ducet=NewVD,
        primary_ducet=NewPD,
        decomp=NewDC
    }.


%% If [Char] is in ducet;
%%      and to_nfd([Char]) is not in ducet,
%%      then delete [Char] from ducet.
array_set([_]=Key1, Weight, Ducet) ->
    Res = case ux_string:to_nfd(Key1) of
        [_Char]=Key2 -> 
            case ux_unidata:ducet(Key2) of
                other -> Ducet;
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
ducet_handler({Key, Bin}, Acc=#acc{
        ducet=Ducet, 
        var_ducet=VarDucet,
        primary_ducet=PrimDucet})
    when is_binary(Bin) -> 
    %% Bin is a binary weight
    {NonVarWeight, VarWeight} = ucol_weights:decode(Bin),
    NewDucet = case ucol_weights:is_empty(NonVarWeight) of
        false -> array_set(Key, NonVarWeight, Ducet);
        true -> array_set(Key, ucol_weights:empty(NonVarWeight), Ducet) end,
    NewVarDucet = case ucol_weights:is_empty(VarWeight) of
        false -> array_set(Key, VarWeight, VarDucet);
        true -> array_set(Key, ucol_weights:empty(VarWeight), VarDucet) end,
    PrimWeight = ucol_weights:primary_weight(NonVarWeight),
    NewPrimDucet = array_set(Key, PrimWeight, PrimDucet),
    Acc#acc{ducet=NewDucet, var_ducet=NewVarDucet, primary_ducet=NewPrimDucet};

ducet_handler(Info, Acc) -> 
    io:format("Info: ~w~n", [Info]), Acc.


blocks_handler({{From, To}, BlockName}, Acc=#acc{implicit=ImpList}) ->
    case ucol_implicit:element(From, To, BlockName) of
        false -> Acc;
        Elem -> Acc#acc{implicit=[Elem|ImpList]}
    end.


finish_blocks(Acc=#acc{ implicit=Imp }) ->
    Acc#acc{ implicit=lists:sort(Imp) }.


%% For decomp table. Change ducet, var_ducet.
decomp_ccc_handler({Char, _ReplaceStr}, CccList) ->
    %% Lets make ccc=DECOMP_CLASS for composed values. 
    %% It is free.
    %% Chars with ccc=DECOMP_CLASS MUST be decomposed manually.
    %%
    %% http://unicode.org/reports/tr44/tr44-4.html#Canonical_Combining_Class_Values
    SpecialCcc = ucol_string:decomp_class(),
    [{Char, SpecialCcc} | proplists:delete(Char, CccList)];

decomp_ccc_handler(_, CccList) -> CccList.


%collect_decomp_handler({_Char, [_ReplaceChar]}, Acc) -> Acc;
collect_decomp_handler({Char, ReplaceStr}, Acc=#acc{decomp=DecompArr}) ->
    Elem = [{X, ux_unidata:ccc(X)} || X <- ReplaceStr],
    NewDecompArr = array:set(Char, Elem, DecompArr),
    Acc#acc{decomp=NewDecompArr}.


forms(#acc{
        ducet=Ducet, 
        var_ducet=VarDucet, 
        primary_ducet=PrimDucet,
        ccc=Ccc, 
        decomp=Decomp,
        implicit=Imp
    }) ->
    {ok, MTs, _} = erl_scan:string("-module(ucol_unidata)."),
    {ok, MF}     = erl_parse:parse_form(MTs),

    AbsDucet     = erl_parse:abstract(Ducet),
    AbsVarDucet  = erl_parse:abstract(VarDucet),
    AbsCcc       = erl_parse:abstract(Ccc),
    AbsDecomp    = erl_parse:abstract(Decomp),
    AbsImp       = erl_parse:abstract(Imp),
    AbsPrimDucet = erl_parse:abstract(PrimDucet),
    ?INFO("Abstract terms were builded."),

    DucetF       = function(ducet, AbsDucet),
    VarDucetF    = function(var_ducet, AbsVarDucet),
    PrimDucetF   = function(primary_ducet, AbsPrimDucet),
    CccF         = function(ccc, AbsCcc),
    DecompF      = function(decomp, AbsDecomp),
    ImpF         = function(implicit, AbsImp),
    ?INFO("Abstract Syntax Trees (AST) were transformed into forms."),

    [MF, CccF, DecompF, ImpF, DucetF, VarDucetF, PrimDucetF].


compile(Acc) ->
    Forms = forms(Acc),
    erlang:garbage_collect(),
    ?INFO("Forms were constructed."),
    compile:forms(Forms, [compressed, export_all, no_line_info, nowarn_format]).


%% Returns forms
function(Name, Result) ->
    erl_syntax:revert(
        % AST
        erl_syntax:function(erl_syntax:atom(Name),
           [erl_syntax:clause([], none, [Result])])).

load() ->
    Acc = ucol_data:generate(),
    ?INFO("Data was extracted from the ux module."),
    {ok, Name, Bin} = ucol_data:compile(Acc),
    ?INFO("The beam module was compiled."),
    code:load_binary(Name, "ucol_unidata", Bin).
