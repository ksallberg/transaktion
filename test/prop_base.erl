 -module(prop_base).

-include_lib("proper/include/proper.hrl").

%% rebar3 proper
%% rebar3 proper -n 10000
%% rebar3 proper -p prop_test
%% rebar3 proper -n 1000 -p prop_test

%% make start
%% > proper_gen:pick(proper_types:string()).
%% > proper_gen:pick(prop_base:table_name()).

-compile(export_all).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
%% What you insert with map_logic will be the same you get back.
prop_set_data() ->
    ?FORALL({Key, Val}, {term(), term()},
            begin
                Map = map_logic:set_data({Key, Val}, #{}),
                ValReturn = maps:get(Key, Map),
                Val =:= ValReturn
            end).

prop_sanity() ->
    ?FORALL({Key, Val, TableName}, {string(), text_like(), table_name()},
            begin
                application:start(transaktion),
                Settings = #{backend => file_backend,
                             name => dbx,
                             disk_name => 'test.db'},
                {ok, Th} = trans_api:create(Settings),
                {ok, key_added, KeyReturn} =
                    trans_api:add(Th, {TableName, {Key, Val}}),
                {ok, value, ValReturn} = trans_api:read(Th, {TableName, Key}),
                {ok, transaction_discarded} = trans_api:discard(Th),
                application:stop(transaktion),
                Key == KeyReturn andalso Val == ValReturn
            end).

%% The map_logic:merge_into concept is critical in that it applies
%% a change set into the currently existing data.
prop_merge_into() ->
    ?FORALL(TableLs, table(),
            begin
                TableCont = [ {TabName, maps:from_list(Content)}
                              || {TabName, Content} <- TableLs],
                Table = maps:from_list(TableCont),
                is_map(Table)
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
is_transaktion_map(#{table1:=T}) ->
    T == #{};
is_transaktion_map(#{}) ->
    true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% Generate an atom that can be used as a table name,
%% that is not the empty atom ''.
table_name() ->
    ?SUCHTHAT(N, atom(), N /= '').

%% stolen from http://propertesting.com/book_custom_generators.html
text_like() ->
    list(frequency([{80, range($a, $z)},
                    {10, $\s},
                    {1,  $\n},
                    {1, oneof([$., $-, $!, $?, $,])},
                    {1, range($0, $9)}
                   ])).

%% Use a generator of proplists to later use these
%% and convert them into maps.
table() ->
    list({table_name(), sub_table()}).

sub_table() ->
    list({table_name(), text_like()}).
