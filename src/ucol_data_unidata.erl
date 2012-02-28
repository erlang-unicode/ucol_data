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
    decomp=array:new(),
    diff_points=ucol_mask:new()
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

    Acc6 = lists:foldl(fun add_decomp_handler/2, Acc5, NewDecompList),
    ?INFO("Composed diff_points were added."),

    fix(Acc6).


fix(Acc=#acc{
    ducet=D,
    var_ducet=VD,
    primary_ducet=PD,
    decomp=DC,
    diff_points=DP
}) -> 
    NewD = ucol_array:fix(D),
    ?INFO("A ducet array was fixed."),

    NewVD = ucol_array:fix(VD),
    ?INFO("A variable ducet array was fixed."),

    NewPD = ucol_array:fix(PD),
    ?INFO("A primary ducet array was fixed."),

    NewDC = array:fix(DC),
    ?INFO("A decomp array was fixed."),

    NewDP = ucol_mask:fix(DP),
    ?INFO("A tree was balanced."),

    Acc#acc{
        ducet=NewD,
        var_ducet=NewVD,
        primary_ducet=NewPD,
        decomp=NewDC,
        diff_points=NewDP
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
        primary_ducet=PrimDucet,
        diff_points=DiffPoints})
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

    %% Create an array of points that are part of group of points.
    %% Example:
    %% Key = [$a]   no points
    %% Key = [X, Y] add X and Y into an array
    NewDiffPoints = case Key of
            %% Add hangul characters (S, L, V) also
            [_] -> 
                case ucol_weights:is_hangul(PrimWeight) of
                    true -> ucol_mask:add(Key, DiffPoints);
                    false -> DiffPoints
                end;

            %% Add difficult points
            [_|_] -> ucol_mask:add_list(Key, DiffPoints)
        end,
    Acc#acc{ducet=NewDucet, var_ducet=NewVarDucet, primary_ducet=NewPrimDucet,
        diff_points=NewDiffPoints};

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

%% Collect mapping Point -> [{Point, CCC}] for composed characters
collect_decomp_handler({Char, ReplaceStr}, Acc=#acc{decomp=DecompArr}) ->
    Elem = [{X, ux_unidata:ccc(X)} || X <- ReplaceStr],
    NewDecompArr = array:set(Char, Elem, DecompArr),
    Acc#acc{decomp=NewDecompArr}.

%% Add decomposed characters
add_decomp_handler({Char, ReplaceStr}, Acc=#acc{diff_points=DiffPoints}) ->
    %% If any of chars in decomposition are difficult, 
    %%    then mark the composed character as difficult too.
    IsDifficult = lists:any(in_mask(DiffPoints), ReplaceStr),
    NewDiffPoints = case IsDifficult of
            true -> ucol_mask:add(Char, DiffPoints);
            false -> DiffPoints
        end,
    Acc#acc{diff_points=NewDiffPoints}.


in_mask(DiffPoints) ->
    fun(Point) ->
        ucol_mask:is_member(Point, DiffPoints)
    end.


forms(#acc{
        ducet=Ducet, 
        var_ducet=VarDucet, 
        primary_ducet=PrimDucet,
        ccc=Ccc, 
        decomp=Decomp,
        implicit=Imp,
        diff_points=DifPoints
    }) ->
    {ok, MTs, _} = erl_scan:string("-module(ucol_unidata)."),
    {ok, MF}     = erl_parse:parse_form(MTs),

    AbsDucet     = erl_parse:abstract(Ducet),
    AbsVarDucet  = erl_parse:abstract(VarDucet),
    AbsCcc       = erl_parse:abstract(Ccc),
    AbsDecomp    = erl_parse:abstract(Decomp),
    AbsImp       = erl_parse:abstract(Imp),
    AbsPrimDucet = erl_parse:abstract(PrimDucet),
    AbsDifPoints = erl_parse:abstract(DifPoints),

    ?INFO("Abstract terms were builded."),

    DucetF       = function(ducet, AbsDucet),
    VarDucetF    = function(var_ducet, AbsVarDucet),
    PrimDucetF   = function(primary_ducet, AbsPrimDucet),
    CccF         = function(ccc, AbsCcc),
    DecompF      = function(decomp, AbsDecomp),
    ImpF         = function(implicit, AbsImp),
    DifPointsF   = function(diff_points, AbsDifPoints),
    ?INFO("Abstract Syntax Trees (AST) were transformed into forms."),

    [MF, CccF, DecompF, ImpF, DucetF, VarDucetF, PrimDucetF, DifPointsF].


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
