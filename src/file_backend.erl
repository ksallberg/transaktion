-module(file_backend).

-behaviour(db_backend).

-export([store/1, read/1]).

-define(DB_FILE, db_file).

store(#{name := DbName, data := Data}) ->
    OldDBCollection =
        case file:read_file(?DB_FILE) of
            {ok, Bin} ->
                %% should have better strategy than
                %% deleting file and writing new
                ok = file:delete(?DB_FILE),
                binary_to_term(Bin);
            {error, _Reason} ->
                #{}
        end,
    NewDBCollection = map_logic:set_data({DbName, Data}, OldDBCollection),
    FinalBin = term_to_binary(NewDBCollection),
    file:write_file(?DB_FILE, FinalBin).

read(DbName) ->
    case file:read_file(?DB_FILE) of
        {ok, Bin} ->
            DBCollection = binary_to_term(Bin),
            case maps:find(DbName, DBCollection) of
                {ok, Tables} ->
                    Tables;
                error ->
                    %% Return inital data structure:
                    #{}
            end;
        {error, _Reason} ->
            %% Return inital data structure:
            #{}
    end.
