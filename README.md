```
$ rebar get-deps
$ rebar compile
$ ./start-dev.sh
> ucol_data:compile().
$ ls -la /tmp/*.beam
-rw-r--r-- 1 user user 458348 Feb 26 16:14 /tmp/ucol_testdata.beam
-rw-r--r-- 1 user user 534664 Feb 26 16:13 /tmp/ucol_unidata.beam
```

`ucol\_data:compile().` will requere 1GB of RAM :)
