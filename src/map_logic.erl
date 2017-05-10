-module(map_logic).

-export([ set_data/3
        , set_trans_oper/3
        , delete_data/3
        , set_data1/2
        ]).

set_trans_oper(TableName, {Key, Oper}, Tables) ->
    set_data(TableName, {Key, Oper}, Tables).

set_data1({Key, Val}, Map) ->
    case maps:find(Key, Map) of
        {ok, _OldData} ->
            maps:update(Key, Val, Map);
        error ->
            maps:put(Key, Val, Map)
    end.

set_data(TableName, {Key, Val}, Tables) ->
    case maps:find(TableName, Tables) of
        {ok, Table} ->
            NewTable = maps:put(Key, Val, Table),
            maps:update(TableName, NewTable, Tables);
        error ->
            NewTable = maps:put(Key, Val, #{}),
            maps:put(TableName, NewTable, Tables)
    end.

delete_data(TableName, Key, Tables) ->
    case maps:find(TableName, Tables) of
        {ok, Table} ->
            NewTable  = maps:remove(Key, Table),
            maps:update(TableName, NewTable, Tables);
        error ->
            {error, no_such_table}
    end.
