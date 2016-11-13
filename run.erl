#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname cougar

-define(SVR, cougar_svr).
-define(DS, data_store).

read_stdin() ->
    save_stdin(io:get_line("")).

save_stdin(eof) -> done;
save_stdin({error, Error}) -> Error;
save_stdin(Data) ->
    ?SVR:save(string:strip(Data, right, $\n)),
    read_stdin().

main([_MaxBufferSize, _FlushTimerTimeout] = Args) ->
    % Compile source file.
    BufferType = case os:getenv("BUFFER_TYPE") of
                     false -> "ets"; Value -> Value
                 end,
    [ c:c(F, {d, list_to_atom(BufferType)}) 
      || F <- filelib:wildcard("cougar*.erl") ],

    % Open Database (data store file) 
    dets:open_file(?DS, [{access, read_write},
                         {auto_save, infinity},
                         {estimated_no_objects, 10000},
                         {ram_file, true},
                         {type, set}]),
    dets:delete_all_objects(?DS),
    
    % Start the buffered data store writer
    ?SVR:start([?DS |[ list_to_integer(A)|| A <- Args ]]),

    % Read and Save data from Standard In.
    read_stdin(),

    % Stop the writer
    ?SVR:stop(),

    % Print out counts
    dets:traverse(?DS, fun({K, V}) -> io:format("~s\t~p~n", [K, V]), continue end),

    % Close data store
    dets:close(?DS);
main(_) ->
    usage().

usage() ->
    io:format("Reads from Standard Input\n"),
    io:format("f.x.: cat 'Filename' | BUFFER_TYPE=dict ./run.erl 100 20\n"),
    halt(1).
