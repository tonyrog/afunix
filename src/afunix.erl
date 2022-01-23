%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(afunix).
-compile(export_all).

-export([connect/2, connect/3, listen/2]). %% afunix special
-export([accept/1, accept/2, close/1]).
-export([send/2, send/3, recv/2, recv/3, unrecv/2]).
-export([shutdown/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).
-export([translate_ip/1]).

-export([get_peercred/1]).
-export([get_peerpid/1]).
-export([get_uid/0]).
-export([get_euid/0]).

-define(FAMILY, local).
%%
%% Connect
%%
connect(Name, Opts) -> 
    local_tcp:connect({local,Name}, 0, Opts).

connect(Name, Opts, Timeout) ->
    local_tcp:connect({local,Name}, 0, Opts, Timeout).

%% 
%% Listen
%%
listen(Name, Opts) ->
    local_tcp:listen(0, [{ifaddr,{local,Name}}|Opts]).

%%
%% Accept
%%
accept(L) -> local_tcp:accept(L).
accept(L,Timeout) -> local_tcp:accept(L, Timeout).

%%
%% Close a socket (async)
%%
close(Socket) -> local_tcp:close(Socket).

%%
%% Send data on a socket
%%
send(Socket, Packet, Opts) -> local_tcp:send(Socket, Packet, Opts).
send(Socket, Packet) -> local_tcp:send(Socket, Packet, []).

%%
%% Receive data from a socket (inactive only)
%%
recv(Socket, Length) -> local_tcp:recv(Socket, Length).
recv(Socket, Length, Timeout) -> local_tcp:recv(Socket, Length, Timeout).

unrecv(Socket, Data) -> local_tcp:unrecv(Socket, Data).

%%
%% Shutdown one end of a socket
%%
shutdown(Socket, How) -> local_tcp:shutdown(Socket, How).
    

%%
%% Set controlling process
%%
controlling_process(Socket, NewOwner) ->
    inet:tcp_controlling_process(Socket, NewOwner). 

%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) -> local_tcp:fdopen(Fd, Opts).

%% port lookup
getserv(0) -> {ok, 0}.

%% no address lookup
getaddr({?FAMILY, _} = Address) -> {ok, Address}.
getaddr({?FAMILY, _} = Address, _Timer) -> {ok, Address}.

%% no address lookup
getaddrs({?FAMILY, _} = Address) -> {ok, [Address]}.
getaddrs({?FAMILY, _} = Address, _Timer) -> {ok, [Address]}.

%% special this side addresses
translate_ip(IP) -> local_udp:translate_ip(IP).


%% get peer-credentials (only effecive uid right now)
get_peercred(S) -> afunix_nif:get_peercred(S).
%% get peer-credentials (only effecive uid right now)
get_peerpid(S) -> afunix_nif:get_peerpid(S).
%% get uid
get_uid() -> afunix_nif:get_uid().
%% get effecive uid right
get_euid() ->afunix_nif:get_euid().
