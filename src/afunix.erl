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
%% Socket server for TCP/IP
-export([connect/2, connect/3, listen/2, accept/1, accept/2, close/1]).
-export([send/2, send/3, recv/2, recv/3, unrecv/2]).
-export([shutdown/2]).
-export([controlling_process/2]).
-export([fdopen/2]).
-export([get_peercred/1]).
-export([get_peerpid/1]).

%% rasperry pi distribute non-source 
%% -include_lib("kernel/src/inet_int.hrl").
-define(INET_REQ_OPEN,          1).
-define(INET_REQ_CONNECT,       3).
-define(INET_REQ_BIND,          6).
-define(INET_REQ_GETOPTS,       8).
-define(INET_REQ_FDOPEN,        13).

-define(INET_REP_ERROR,    0).
-define(INET_REP_OK,       1).
-define(INET_REP,          2).

-define(int32(X),
        [((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
         ((X) bsr 8) band 16#ff, (X) band 16#ff]).
-define(u16(X1,X0),
        (((X1) bsl 8) bor (X0))).
-define(u32(X3,X2,X1,X0),
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(INET_AF_INET,         1).
-define(INET_AF_INET6,        2).
-define(INET_AF_UNIX, 5).

-define(INET_TYPE_STREAM,     1).
-define(INET_TYPE_DGRAM,      2).
-define(INET_TYPE_SEQPACKET,  3).

-define(UNIX_OPT_PEERCRED,  101).
-define(UNIX_OPT_PEERPID,   102).

-define(LISTEN_BACKLOG, 5).     %% default backlog 

-record(connect_opts,
	{
	  ifaddr = any,     %% bind to interface address
          port   = 0,       %% bind to port (default is dynamic port)
          fd      = -1,     %% fd >= 0 => already bound
          opts   = []       %% [{active,true}] added in inet:connect_options 
         }).

-record(listen_opts,
        {
          ifaddr = any,              %% bind to interface address
          port   = 0,                %% bind to port (default is dynamic port)
          backlog = ?LISTEN_BACKLOG, %% backlog
          fd      = -1,              %% %% fd >= 0 => already bound 
          opts   = []                %% [{active,true}] added in
                                     %% inet:listen_options 
         }).

%-define(DEBUG, 1).
-ifdef(DEBUG).
-define(DBG_FORMAT(Format, Args), (io:format((Format), (Args)))).
-else.
-define(DBG_FORMAT(Format, Args), ok).
-endif.

load(Driver) ->
    Path = code:priv_dir(afunix),
    case erl_ddll:load(Path, Driver) of
	ok -> 
	    ok;
	Err={error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    Err
    end.

%%
%% Send data on a socket
%%
send(Socket, Packet, Opts) -> prim_inet:send(Socket, Packet, Opts).
send(Socket, Packet) -> prim_inet:send(Socket, Packet, []).

%%
%% Receive data from a socket (inactive only)
%%
recv(Socket, Length) -> prim_inet:recv(Socket, Length).
recv(Socket, Length, Timeout) -> prim_inet:recv(Socket, Length, Timeout).

unrecv(Socket, Data) -> prim_inet:unrecv(Socket, Data).

%%
%% Shutdown one end of a socket
%%
shutdown(Socket, How) ->
    prim_inet:shutdown(Socket, How).
    
%%
%% Close a socket (async)
%%
close(Socket) -> 
    inet:tcp_close(Socket).

%%
%% Set controlling process
%%
controlling_process(Socket, NewOwner) ->
    inet:tcp_controlling_process(Socket, NewOwner). 

%%
%% Connect
%%
connect(Name, Opts) ->
    do_connect(Name, Opts, infinity).

connect(Name, Opts, infinity) ->
    do_connect(Name, Opts, infinity);
connect(Name, Opts, Timeout) when is_integer(Timeout), 
                                           Timeout >= 0 ->
    do_connect(Name, Opts, Timeout).

do_connect(Name, Opts, Time) when is_list(Name) ->
    case inet:connect_options(Opts, inet) of
	{error, Reason} -> exit(Reason);
	{ok, #connect_opts{fd=Fd,
			   opts=SockOpts}} ->
	    case open(Fd,"",SockOpts,unix,afunix,stream,?MODULE) of
		{ok, S} ->
		    case prim_connect(S, Name, Time) of
			ok    -> {ok,S};
			Error ->  prim_inet:close(S), Error
		    end;
		Error -> Error
	    end
    end.

%% 
%% Listen
%%
listen(Name, Opts) when is_list(Name) ->  %% or binary?
    case inet:listen_options(Opts, inet) of
	{error,Reason} -> exit(Reason);
	{ok, #listen_opts{fd=Fd,
			  opts=SockOpts}=R} ->
	    %% unlink Name!!?
	    case open(Fd,Name,SockOpts,unix,afunix,stream,?MODULE) of
		{ok, S} ->
		    case prim_inet:listen(S, R#listen_opts.backlog) of
			ok -> {ok, S};
			Error -> prim_inet:close(S), Error
		    end;
		Error -> Error
	    end;
	{ok, _} -> exit(badarg)
    end.

%%
%% Accept
%%
accept(L) -> 
    case prim_inet:accept(L) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.
	    
accept(L,Timeout) -> 
    case prim_inet:accept(L,Timeout) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.
%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    fdopen(Fd, Opts, unix, afunix, stream, ?MODULE).


fdopen(Fd, Opts, Protocol, Family, Type, Module) ->
    case prim_fdopen(Protocol, Family, Type, Fd) of
	{ok, S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    inet_db:register_socket(S, Module),
		    {ok, S};
		Error ->
		    prim_inet:close(S), Error
	    end;
	Error -> Error
    end.

open(Fd, Name, Opts, Protocol, Family, Type, Module) when Fd < 0 ->
    case prim_open(Protocol, Family, Type) of
	{ok,S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    case prim_bind(S, Name) of
			{ok, _} -> 
			    inet_db:register_socket(S, Module),
			    {ok,S};
			Error  ->
			    prim_inet:close(S),
			    Error
		    end;
		Error  ->
		    prim_inet:close(S),
		    Error
	    end;
	Error ->
	    Error
    end;
open(Fd, _Name, Opts, Protocol, Family, Type, Module) ->
    fdopen(Fd, Opts, Protocol, Family, Type, Module).


prim_open(Protocol, Family, Type) ->
    open0(Protocol, Family, Type, ?INET_REQ_OPEN, []).

prim_fdopen(Protocol, Family, Type, Fd) when is_integer(Fd) ->
    open0(Protocol, Family, Type, ?INET_REQ_FDOPEN, ?int32(Fd)).


open0(Protocol, Family, Type, Req, Data) ->
    Drv = protocol2drv(Protocol),
    AF = enc_family(Family),
    T = enc_type(Type),
    case load(Drv) of
	ok ->
	    try erlang:open_port({spawn_driver,Drv}, [binary]) of
		S ->
		    case ctl_cmd(S, Req, [AF,T,Data]) of
			{ok,_} -> {ok,S};
			{error,_}=Error ->
			    close(S),
			    Error
		    end
	    catch
		%% The only (?) way to get here is to try to open
		%% the sctp driver when it does not exist (badarg)
		error:badarg       -> {error, eprotonosupport};
		%% system_limit if out of port slots
		error:system_limit -> {error, system_limit}
	    end;
	Error ->
	    Error
    end.

enc_family(inet) -> ?INET_AF_INET;
enc_family(inet6) -> ?INET_AF_INET6;
enc_family(afunix) -> ?INET_AF_UNIX.

enc_type(stream) -> ?INET_TYPE_STREAM;
enc_type(dgram) -> ?INET_TYPE_DGRAM;
enc_type(seqpacket) -> ?INET_TYPE_SEQPACKET.

enc_time(Time) when Time < 0 -> [255,255,255,255];
enc_time(Time) -> ?int32(Time).

protocol2drv(tcp)  -> "tcp_inet";
protocol2drv(udp)  -> "udp_inet";
protocol2drv(sctp) -> "sctp_inet";
protocol2drv(unix)  -> "afunix_drv".

%% drv2protocol("tcp_inet")  -> tcp;
%% drv2protocol("udp_inet")  -> udp;
%% drv2protocol("sctp_inet") -> sctp;
%% drv2protocol("afunix_drv") -> unix;
%% drv2protocol(_)           -> undefined.

%% prim_connect(S, Name) -> connect0(S, Name, -1).

prim_connect(S, Name, infinity) -> connect0(S, Name, -1);
prim_connect(S, Name, Time)     -> connect0(S, Name, Time).

connect0(S, Name, Time) when is_port(S), is_list(Name), is_integer(Time) ->
    case async_connect(S, Name, Time) of
	{ok, S, Ref} ->
	    receive
		{inet_async, S, Ref, Status} ->
		    Status
	    end;
	Error -> Error
    end.

async_connect(S, Name, Time) ->
    case ctl_cmd(S, ?INET_REQ_CONNECT,
		 [enc_time(Time),Name]) of
	{ok, [R1,R0]} -> {ok, S, ?u16(R1,R0)};
	{error,_}=Error -> Error
    end.

%% get peer-credentials (only effecive uid right now)
get_peercred(S) ->
    case ctl_cmd(S, ?INET_REQ_GETOPTS, [?UNIX_OPT_PEERCRED]) of
	{ok,[?UNIX_OPT_PEERCRED,U3,U2,U1,U0]} ->
	    {ok, ?u32(U3,U2,U1,U0)};
	{ok, []} ->
	    {error, einval};
	{error,_}=Error -> Error
    end.
%% get peer-credentials (only effecive uid right now)
get_peerpid(S) ->
    case ctl_cmd(S, ?INET_REQ_GETOPTS, [?UNIX_OPT_PEERPID]) of
	{ok,[?UNIX_OPT_PEERPID,U3,U2,U1,U0]} ->
	    {ok, ?u32(U3,U2,U1,U0)};
	{ok, []} ->
	    {error, einval};
	{error,_}=Error -> Error
    end.

prim_bind(S,"") when is_port(S) ->
    {ok,""};
prim_bind(S,Name) when is_port(S), is_list(Name) ->
    case ctl_cmd(S,?INET_REQ_BIND, [?INET_AF_UNIX, Name]) of
	{ok,_} -> {ok,Name};
	{error,_}=Error -> Error
    end.

%% Control command
ctl_cmd(Port, Cmd, Args) ->
    ?DBG_FORMAT("prim_inet:ctl_cmd(~p, ~p, ~p)~n", [Port,Cmd,Args]),
    Result =
	try erlang:port_control(Port, Cmd, Args) of
	    [?INET_REP_OK|Reply]  -> {ok,Reply};
%%	    [?INET_REP]  -> inet_reply;
	    [?INET_REP_ERROR|Err] -> {error,list_to_atom(Err)}
	catch
	    error:_               -> {error,einval}
	end,
        ?DBG_FORMAT("prim_inet:ctl_cmd() -> ~p~n", [Result]),
    Result.
