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

%% db_trans is the representation of a transaction.
%% it is used to branch out from the central state of db_core.
%%
%% +------------+
%% |  db_trans  |
%% +------------+
%%      |
%%      v
%% +-----------+
%% |  db_core  |
%% +-----------+
%%
%% The representation is a map in a map, similar to the representation in
%% db_core. However, instead of a datavalue, db_trans instead keeps a
%% transaction operation as the value.
%% map(key=tablename, value=map(key=keyname, value=trans_operation))
%%
%% The trans_operation is either '{set, data}', or 'delete'.

-module(db_trans).

-author('kristian@purestyle.se').

-behaviour(gen_server).

-export([start_link/1]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

% State while receiving bytes from the tcp socket
-record(state, { flags        :: integer()
               , changeset    :: map()
               , options      :: map()
               }).

-type state() :: #state{}.

-define(TIMEOUT, infinity).

%% Interface: __________________________________________________________________

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% Gen Serv:  __________________________________________________________________

init(Options) ->
    lager:log(info, self(), "New transaction created", []),
    {ok, #state{changeset = #{}, options = Options}}.

-spec handle_cast({data, string()} | timeout | {socket_ready, port()}, state())
    -> {stop, normal, state()} | {noreply, state(), infinity}.
handle_cast(debug, #state{changeset = Tables} = State) ->
    debug(Tables),
    {noreply, State};

handle_cast(debug_dryrun, #state{changeset = ChangeSet,
                                 options = Options} = State) ->
    BaseTables = gen_server:call(db_core, {read, Options}),
    Merged = map_logic:merge_into(BaseTables, ChangeSet),
    debug(Merged),
    {noreply, State};

handle_cast(timeout, State) ->
    {stop, normal, State}.

-spec handle_call(any(), {pid(), any()}, state()) -> {stop, tuple(), state()}.
handle_call(discard, _From, State) ->
    {stop, normal, State};

handle_call({add, Tab, Key, Val}, _From, #state{changeset = Tables} = State) ->
    NewTables = map_logic:set_data(Tab, {Key, {set, Val}}, Tables),
    {reply, key_added, State#state{changeset = NewTables}};

handle_call({delete, Tab, Key}, _From, #state{changeset = Tables} = State) ->
    NewTables = map_logic:set_data(Tab, {Key, delete}, Tables),
    {reply, key_deleted, State#state{changeset = NewTables}};

handle_call({read, TableName, Key}, _From,
            #state{changeset = ChgSet,
                   options   = Options} = State) ->
    BaseTables = gen_server:call(db_core, {read, Options}),
    case map_logic:get_data(TableName, Key, ChgSet, BaseTables) of
        {error, not_existing} ->
            {reply, error, State};
        Result ->
            {reply, {kv_pair, {Key, Result}}, State}
    end;

handle_call(commit, _From, #state{changeset = Tables,
                                  options   = Options} = State) ->
    CommitResult = gen_server:call(db_core, {commit, Tables, Options}),
    {reply, CommitResult, State};

handle_call(Request, _From, State) ->
    {stop, {Request, undefined_event}, State}.

-spec handle_info(any(), state() | port()) -> {noreply, state()} |
                                              {stop, normal, state()} |
                                              {noreply, state(), infinity}.
handle_info(_Info, StateData) ->
    {noreply, StateData}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{} = _State) ->
    ok.

%% For now, just return the received state data
-spec code_change(atom(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

debug(Tables) ->
    TablesLs = maps:to_list(Tables),
    DebugFInner = fun({Key, Value}) ->
                          lager:log(info, self(),
                                    "  Key: ~p, Value: ~p", [Key, Value])
                  end,
    DebugF = fun({TableName, TableData}) ->
                     TableDataLs = maps:to_list(TableData),
                     lager:log(info, self(), "Table: ~p", [TableName]),
                     lists:foreach(DebugFInner, TableDataLs)
             end,
    lists:foreach(DebugF, TablesLs).
