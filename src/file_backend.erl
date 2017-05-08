-module(file_backend).

-behaviour(db_backend).

-export([store/1, read/1]).

store(#{name := Name, data := Data}) ->
    Bin = term_to_binary(Data),
    file:write_file(Name, Bin).

read(Name) ->
    case file:read_file(Name) of
        {ok, Bin} ->
            binary_to_term(Bin);
        {error, _Reason} ->
            file:write_file(Name, <<"">>),
            %% Return inital data structure:
            #{}
    end.
