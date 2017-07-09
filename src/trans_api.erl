%% Copyright (c) 2017, Kristian Sällberg
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice, this
%%   list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
%% OF THE POSSIBILITY OF SUCH DAMAGE.

-module(trans_api).

-author('kristian@purestyle.se').

-export([ create/1
        , discard/1
        , commit/1
        , add/2
        , delete/2
        , read/2
        , debug/1
        , debug_merge/1
        , multi_call/1 ]).

-export([ join/1
        , debug_other_nodes/0]).

-type transaction() :: pid().

%% Delegate creation to the supervisor
-spec create(map()) -> transaction().
create(Options) ->
    db_trans_sup:create(Options).

discard(Th) ->
    try
        gen_server:call(Th, discard)
    catch exit:{normal, {gen_server, call, _}} ->
            {ok, transaction_discarded}
    end.

add(Th, {Table, {Key, Val}}) ->
    case gen_server:call(Th, {add, Table, Key, Val}) of
        key_added ->
            {ok, key_added, Key};
        Err ->
            {error, Err}
    end.

delete(Th, {Table, Key}) ->
    case gen_server:call(Th, {delete, Table, Key}) of
        key_deleted ->
            {ok, key_deleted, Key};
        Err ->
            {error, Err}
    end.

read(Th, {Table, Key}) ->
    case gen_server:call(Th, {read, Table, Key}) of
        {kv_pair, {_Key, Value}} ->
            {ok, value, Value};
        Err when Err == error ->
            {error, not_existing}
    end.

commit(Th) ->
    case gen_server:call(Th, commit) of
        commited ->
            {ok, commited};
        commit_failed ->
            {error, commit_failed};
        Err ->
            {error, Err}
    end.

debug(Th) ->
    gen_server:cast(Th, debug).

debug_merge(Th) ->
    gen_server:cast(Th, debug_dryrun).

%%%%%%%%%%%%%%%%%%
%% Distribution %%
%%%%%%%%%%%%%%%%%%

join(Node) ->
    Res = net_adm:ping(Node),
    case Res of
        pong ->
            Nodes = nodes(),
            io:format("Initially seen: ~B~n", [length(Nodes)]);
        pang ->
            io:format("Error, could not connect to ~p. ~n", [Node])
    end,
    ok.

multi_call(Msg) ->
    gen_server:multi_call(nodes(), db_core, Msg).

%% sort to make testing easier
debug_other_nodes() ->
    Nodes = lists:sort(nodes()),
    io:format("Len Of Nodes: ~B~n", [length(Nodes)]),
    io:format("Other Nodes: ~p~n", [Nodes]).
