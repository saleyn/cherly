%%====================================================================
%%
%% Cherly
%%
%% Copyright (c) 2012 Stoic, Inc.
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
%% Cherly Unit Test
%% @doc
%% @end
%%====================================================================
-module(test_cherly).
-author('Yoshiyuki Kanno').
-vsn('0.9.1').
-include_lib("eunit/include/eunit.hrl").

-export([succ/1, fast_acc/3, time_to_epoch_float/1]).

simple_test() ->
    C = cherly:start(120),
    Value = <<"value">>,
    cherly:put(C, "key", Value),
    ?assertEqual(Value, cherly:get(C, "key")),
    ?assertEqual(9, cherly:size(C)),
    cherly:stop(C).

put_get_and_remove_test() ->
    C = cherly:start(120),
    Value = <<"value">>,
    ?assertEqual(none, cherly:get(C, "key")),
    cherly:put(C, "key", Value),
    ?assertEqual(Value, cherly:get(C, "key")),
    cherly:remove(C, "key"),
    ?assertEqual(none, cherly:get(C, "key")),
    ?assertEqual(0, cherly:size(C)),
    cherly:stop(C).

put_with_lru_eject_test() ->
    C = cherly:start(70),
    Value = <<"value">>,
    lists:foldl(fun(_, Str) ->
                        Mod = succ(Str),
                        cherly:put(C, Mod, Value),
                        Mod
                end, "abc", lists:seq(1, 10)),
    ?debugVal(cherly:size(C)),
    ?debugVal(cherly:items(C)),
    %% ?assertEqual(72, cherly:size(C)),
    %% ?assertEqual(8, cherly:items(C)),
    cherly:stop(C).

what_goes_in_must_come_out_test() ->
    C = cherly:start(120),
    cherly:put(C, "key", list_to_binary([<<"val1">>, <<"val2">>])),
    ?assertEqual(list_to_binary([<<"val1">>, <<"val2">>]), cherly:get(C, "key")),
    cherly:stop(C).

big_stuff_that_goes_in_must_come_out_test() ->
    C = cherly:start(1048576),
    V1 = <<0:524288>>,
    V2 = <<1:524288>>,
    cherly:put(C, "key", list_to_binary([V1, V2])),
    Ret = cherly:get(C, "key"),
    ?assertEqual(list_to_binary([V1,V2]), Ret),
    cherly:stop(C).

put_one_thing_in_no_list_big_test() ->
    C = cherly:start(1048576),
    V = <<0:524288>>,
    cherly:put(C, "key", V),
    ?assertEqual(V, cherly:get(C, "key")),
    cherly:stop(C).

put_one_thing_in_no_list_small_test() ->
    C = cherly:start(1048576),
    V = <<1:8>>,
    cherly:put(C, "key", V),
    ?assertEqual(V, cherly:get(C, "key")),
    cherly:stop(C).

remove_nonexistant_test() ->
    C = cherly:start(120),
    cherly:remove(C, "key"),
    ?assertEqual(none, cherly:get(C, "key")),
    cherly:stop(C).

double_get_test() ->
    %% outputv modifies the iovec with a skipsize.  That's fucking rad
    C = cherly:start(1123123),
    Val = list_to_binary([<<131,108,0,0,0,1,104,2,107,0,9,60,48,46,52,55,50,46,48,
                            62,99,49,46,50,51,54,53,51,49,53,54,49,57,53,57,56,55,
                            50,57,54,49,48,52,101,43,48,57,0,0,0,0,0,106>>,
                          <<235,105,34,223,191,105,56,25,199,24,148,52,180,112,
                            198,246,56,150,15,175,56,34,38,120,99,41,59,53,204,
                            233,41,246,189,135,39,171,124,233,143,40,108,119,63,
                            130,237,8,121,35,97,121,172,20,149,241,129,191,2,211,
                            151,167,0,102,103,63,242,240,41,83,150,211,189,32,56,
                            65,217,241,234,237,58,216,34,245,253,153,140,190,186,
                            24,147,240,181,63,222,161,13,217,55,232,254,148>>]),
    cherly:put(C, "aczup", Val),
    ?assertEqual(Val, cherly:get(C, "aczup")),
    ?assertEqual(Val, cherly:get(C, "aczup")),
    cherly:stop(C).

succ([]) ->
    [];

succ(Str) ->
    succ_int(lists:reverse(Str), []).

succ_int([Char|Str], Acc) ->
    if
        Char >= $z -> succ_int(Str, [$a|Acc]);
        true -> lists:reverse(lists:reverse([Char+1|Acc]) ++ Str)
    end.

fast_acc(_, Acc, 0) -> Acc;

fast_acc(Fun, Acc, N) ->
    fast_acc(Fun, Fun(Acc), N-1).

time_to_epoch_float({Mega,Sec,Micro}) ->
    Mega * 1000000 + Sec + Micro / 1000000.
