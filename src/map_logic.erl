-module(map_logic).

-export([ set_data/2
        , set_data/3
        , set_trans_oper/3
        , merge_into/2
        , get_data/4
        ]).

set_trans_oper(TableName, {Key, Oper}, Tables) ->
    set_data(TableName, {Key, Oper}, Tables).

set_data({Key, Val}, Map) ->
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

%% When reading, first try to find the value from the transaction's
%% change set. If that is not existing, then look deeper, into the
%% base that the transactions branches out from.
get_data(TableName, Key, ChangeSet, Base) ->
    case get_data(TableName, ChangeSet) of
        {error, not_existing} ->
            case get_data(TableName, Base) of
                {error, not_existing}
                    ->
                    {error, not_existing};
                TableInBase ->
                    get_data(Key, TableInBase)
            end;
        Table ->
            get_data(Key, Table)
    end.

get_data(Key, Table) ->
    case maps:find(Key, Table) of
        %% Implementation detail, hide 'set' from the api
        {ok, {set, Value}} ->
            Value;
        {ok, delete} ->
            {error, not_existing};
        {ok, Value} ->
            Value;
        error ->
            {error, not_existing}
    end.

merge_into(BaseTables, Tables) ->
    try
        merge_into1(BaseTables, Tables)
    catch
        throw:{error, delete_non_existing} ->
            {error, delete_non_existing}
    end.

merge_into1(BaseTables, Tables) ->
    FF = fun(TableName, TransTable, Acc) ->
                 CurrentBaseTable =
                     case maps:find(TableName, Acc) of
                         error ->
                             #{};
                         {ok, ExistingTable} ->
                             ExistingTable
                     end,
                 NewBaseTable = merge_into2(CurrentBaseTable, TransTable),
                 set_data({TableName, NewBaseTable}, Acc)
         end,
    maps:fold(FF, BaseTables, Tables).

merge_into2(BaseTable, TransTable) ->
    FF = fun(Key, delete, Acc) ->
                 case maps:find(Key, Acc) of
                     error ->
                         throw({error, delete_non_existing});
                     {ok, _Val} ->
                         maps:remove(Key, Acc)
                 end;
            (Key, {set, Value}, Acc) ->
                 set_data({Key, Value}, Acc)
         end,
    maps:fold(FF, BaseTable, TransTable).
