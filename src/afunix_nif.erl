%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Special NIF
%%% @end
%%% Created : 22 Jan 2022 by Tony Rogvall <tony@rogvall.se>

-module(afunix_nif).

-export([get_peercred/1]).
-export([get_peerpid/1]).
-export([get_uid/0]).
-export([get_euid/0]).

%% internal
-export([get_peercred_/1]).
-export([get_peerpid_/1]).
-export([get_uid_/0]).
-export([get_euid_/0]).

-on_load(init/0).

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    ok = erlang:load_nif(filename:join(code:priv_dir(afunix), ?MODULE), 0).

get_peercred(Socket) ->
    {ok,Fd} = prim_inet:getfd(Socket),
    get_peercred_(Fd).

get_peerpid(Socket) ->
    {ok,Fd} = prim_inet:getfd(Socket),
    get_peerpid_(Fd).

get_uid() ->
    get_uid_().

get_euid() ->
    get_euid_().

get_peercred_(_Fd) -> ?nif_stub().
get_peerpid_(_Fd) -> ?nif_stub().
get_uid_() -> ?nif_stub().
get_euid_() -> ?nif_stub().

    


