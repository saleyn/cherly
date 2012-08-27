%%%-------------------------------------------------------------------
%%% File:      cherly.erl
%%% @author    Cliff Moon <cliff@moonpolysoft.com> []
%%% @copyright 2009 Cliff Moon See LICENSE file
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-22 by Cliff Moon
%%% @since 2012-02-22 by Yoshiyuki Kanno
%%%-------------------------------------------------------------------
-module(cherly).
-author('cliff@moonpolysoft.com').
-author('Yoshiyuki Kanno').

-export([start/1, put/3, get/2, remove/2, size/1, items/1, stop/1]).
-on_load(init/0).

init() ->
    SoName = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case code:which(?MODULE) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename),"../priv", "cherly"]);
                         _ ->
                             filename:join("../priv", "cherly")
                     end;
                 Dir ->
                     filename:join(Dir, "cherly")
             end,
    erlang:load_nif(SoName, 0).

%% api fallbacks
%%
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

