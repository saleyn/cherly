%% -------------------------------------------------------------------
%%
%% Cherly - Benchmarking Suite
%%
%% Copyright (c) 2012 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_bench_driver_cherly).
-author("Yosuke Hara").

-export([new/1,
         run/4]).

-record(state, {handler :: binary()}).

%% ====================================================================
%% API
%% ====================================================================

new(_Id) ->
    case code:which(cherly) of
        non_existing ->
            io:format("Cherly-benchmark requires cherly to be available on code path.\n");
        _ ->
            void
    end,

    CacheCapacity = basho_bench_config:get(
                      cache_capacity, 1073741824), %% default:1GB
    io:format("Cache capacity: ~w\n", [CacheCapacity]),

    {ok, C} = cherly:start(CacheCapacity),
    {ok, #state{handler = C}}.


run(get, KeyGen, _ValueGen, #state{handler = C} = State) ->
    case cherly:get(C, KeyGen()) of
        {ok, _Value} ->
            {ok, State};
        not_found ->
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end;

run(put, KeyGen, ValueGen, #state{handler = C} = State) ->
    case cherly:put(C, KeyGen(), ValueGen()) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

