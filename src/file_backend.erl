%% Sample file structure
%% #{dbX => #{tab => #{k1 => v2}}}

-module(file_backend).

-behaviour(db_backend).

-export([store/2, read/1]).

-define(DB_FILE, db_file).

store(#{name := DbName, data := Data} = _StoreInfo,
      #{disk_name := DiskFileName}    = _Options) ->
    OldDBCollection =
        case file:read_file(DiskFileName) of
            {ok, Bin} ->
                %% should have better strategy than
                %% deleting file and writing new
                ok = file:delete(DiskFileName),
                binary_to_term(Bin);
            {error, _Reason} ->
                #{}
        end,
    NewDBCollection = map_logic:set_data({DbName, Data}, OldDBCollection),
    FinalBin = term_to_binary(NewDBCollection),
    file:write_file(DiskFileName, FinalBin).

%% Returns only the tables of a given DB
read(#{name := DbName, disk_name := DiskFileName}) ->
    case file:read_file(DiskFileName) of
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
