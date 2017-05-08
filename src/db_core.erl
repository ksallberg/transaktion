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

-module(db_core).

-author('kristian@purestyle.se').

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% State while receiving bytes from the tcp socket
-record(state, { flags         :: integer()
               }).

-type state() :: #state{}.

-define(TIMEOUT, infinity).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_Whatever) ->
    erlang:register(db_core, self()),
    lager:log(info, self(), "db_core running...", []),
    {ok, #state{}}.

-spec handle_cast({data, string()} | timeout | {socket_ready, port()}, state())
    -> {stop, normal, state()} | {noreply, state(), infinity}.
handle_cast(timeout, State) ->
    error_logger:error_msg("~p Client connection timeout.~n", [self()]),
    {stop, normal, State}.

-spec handle_call(any(), {pid(), any()}, state()) -> {stop, tuple(), state()}.
handle_call({commit, Tables, #{backend := Backend, name := Name}},
            _From, State) ->
    lager:log(info, self(), "db_core commit", []),
    ok = apply(Backend, store, [#{name => Name, data => Tables}]),
    {reply, commited, State};

handle_call({read, #{backend := Backend, name := Name}}, _From, State) ->
    Data = apply(Backend, read, [Name]),
    {reply, Data, State};

handle_call(Request, _From, State) ->
    {stop, {Request, undefined_event}, State}.

-spec handle_info(any(), state() | port()) -> {noreply, state()} |
                                              {stop, normal, state()} |
                                              {noreply, state(), infinity}.
handle_info(Info, StateData) ->
    lager:log(info, self(), "Unknown message received: ~p", [Info]),
    {noreply, StateData}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{flags  = _Flags} = _State) ->
    ok.

%% For now, just return the received state data
-spec code_change(atom(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.
