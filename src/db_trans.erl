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

-module(db_trans).

-author('kristian@purestyle.se').

-behaviour(gen_server).

-export([start_link/0]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

% State while receiving bytes from the tcp socket
-record(state, { flags     :: integer()
               , data = [] :: [term()]
               }).

-type state() :: #state{}.

-define(TIMEOUT, infinity).

%% Interface: __________________________________________________________________

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% Gen Serv:  __________________________________________________________________

init(_Whatever) ->
    lager:log(info, self(), "New transaction created", []),
    {ok, #state{}}.

-spec handle_cast({data, string()} | timeout | {socket_ready, port()}, state())
    -> {stop, normal, state()} | {noreply, state(), infinity}.
handle_cast(timeout, State) ->
    {stop, normal, State}.

-spec handle_call(any(), {pid(), any()}, state()) -> {stop, tuple(), state()}.
handle_call(discard, _From, State) ->
    {stop, normal, State};

handle_call({add, Key, Val}, _From, #state{data = Data} = State) ->
    NewData = Data ++ [{Key, Val}],
    {reply, key_added, State#state{data = NewData}};

handle_call({delete, Key}, _From, #state{data = Data} = State) ->
    NewData = [{StoredKey, Value} ||
                  {StoredKey, Value} <- Data, StoredKey /= Key],
    {reply, key_deleted, State#state{data = NewData}};

handle_call({read, Key}, _From, #state{data = Data} = State) ->
    Result = [Value ||
                  {StoredKey, Value} <- Data, StoredKey == Key],
    {reply, {kv_pair, Result}, State};

handle_call(commit, _From, #state{data = Data} = State) ->
    CommitResult = gen_server:call(db_core, {commit, Data}),
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
