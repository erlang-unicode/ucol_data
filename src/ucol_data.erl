-module(ucol_data).
%% API
-export([compile/0]).

-define(INFO(Str), error_logger:info_msg("[" ++ ?MODULE_STRING ++ "] " ++ Str)).

compile() ->
    {ok, ucol_unidata, UnidataBin} = 
        ucol_data_unidata:compile(ucol_data_unidata:generate()),
    ?INFO("Unidata module was generated."),

    file:write_file("/tmp/ucol_unidata.beam", UnidataBin),
    ?INFO("Unidata module was written."),

    gc(),

    {ok, ucol_testdata, TestBin} = 
        ucol_data_testdata:compile(ucol_data_testdata:generate()),
    ?INFO("Test module was generated."),

    file:write_file("/tmp/ucol_testdata.beam", TestBin),
    ?INFO("Test module was written.").


gc() ->
    erlang:garbage_collect(),
    ?INFO("Garbage collection was completed.").
