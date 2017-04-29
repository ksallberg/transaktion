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
        , read/2]).

-type transaction() :: pid().

%% Delegate creation to the supervisor
-spec create([term()]) -> transaction().
create(_Options) ->
    db_trans_sup:create().

discard(Th) ->
    try
        gen_server:call(Th, discard)
    catch exit:{normal, {gen_server, call, _}} ->
            {ok, transaction_discarded}
    end.

add(Th, {_Table, {Key, Val}}) ->
    case gen_server:call(Th, {add, Key, Val}) of
        key_added ->
            {ok, key_added, Key};
        Err ->
            {error, Err}
    end.

delete(Th, {_Table, Key}) ->
    case gen_server:call(Th, {delete, Key}) of
        key_deleted ->
            {ok, key_deleted, Key};
        Err ->
            {error, Err}
    end.

read(Th, {_Table, Key}) ->
    case gen_server:call(Th, {read, Key}) of
        {kv_pair, []} ->
            {error, no_result};
        {kv_pair, Result} ->
            {ok, value, Result};
        Err ->
            {error, Err}
    end.

commit(Th) ->
    case gen_server:call(Th, commit) of
        commited ->
            {ok, commited};
        no_op ->
            {error, no_op};
        val_error ->
            {error, val_error};
        Err ->
            {error, Err}
    end.
