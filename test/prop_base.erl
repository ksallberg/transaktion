-module(prop_base).
-include_lib("proper/include/proper.hrl").

%% rebar3 proper
%% rebar3 proper -n 10000
%% rebar3 proper -p prop_test
%% rebar3 proper -n 1000 -p prop_test

%% make start
%% > proper_gen:pick(proper_types:string()).
%% > proper_gen:pick(prop_base:wordy_string()).
%% > proper_gen:pick(prop_base:word()).

-compile(export_all).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Type, term(),
            begin
                boolean(Type)
            end).

%% What you insert with map_logic will be the same you get back.
prop_set_data() ->
    ?FORALL({Key, Val}, {term(), term()},
            begin
                Map = map_logic:set_data({Key, Val}, #{}),
                Val2 = maps:get(Key, Map),
                io:format("hej ~p ~p ~n", [Val, Val2]),
                Val =:= Val2
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
boolean(aa) -> false;
boolean(_) -> true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
mytype() -> term().
