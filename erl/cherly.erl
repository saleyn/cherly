%%%-------------------------------------------------------------------
%%% File:      cherly.erl
%%% @author    Cliff Moon <cliff@moonpolysoft.com> []
%%% @copyright 2009 Cliff Moon See LICENSE file
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-22 by Cliff Moon
%%%-------------------------------------------------------------------
-module(cherly).
-author('cliff@moonpolysoft.com').

-export([start/1, put/3, get/2, remove/2, size/1, items/1, stop/1]).
-on_load(init/0).

-ifdef(TEST).
-include("test_cherly.erl").
-endif.

init() ->
  ok = load_driver().

%% api fallbacks

start(_Size) ->
  exit(nif_library_not_loaded).

put(_Res, _Key, _Value) ->
  exit(nif_library_not_loaded).
  
get(_Res, _Key) ->
  exit(nif_library_not_loaded).
  
remove(_Res, _Key) ->
  exit(nif_library_not_loaded).
  
size(_Res) ->
  exit(nif_library_not_loaded).
  
items(_Res) ->
  exit(nif_library_not_loaded).
  
stop(_Res) ->
  exit(nif_library_not_loaded).

%%====================================================================
%% Internal functions
%%====================================================================
  
load_driver() ->
  Path = filename:join([filename:dirname(code:which(cherly)), "..", "priv", ?MODULE]),
  erlang:load_nif(Path, 0).
  
