-module(ucol_data).
-export([compile/0]).

compile() ->
    {ok, ucol_unidata, UnidataBin} = 
        ucol_data_unidata:compile(ucol_data_unidata:generate()),
    {ok, ucol_testdata, TestBin} = 
        ucol_data_testdata:compile(ucol_data_testdata:generate()),
    file:write_file("/tmp/ucol_unidata.beam", UnidataBin),
    file:write_file("/tmp/ucol_testdata.beam", TestBin).
