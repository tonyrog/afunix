%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
-module(afunix_api_SUITE).

%% Tests the documented API for the gen_tcp functions.  The "normal" cases
%% are not tested here, because they are tested indirectly in this and
%% and other test suites.

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,
	 t_accept_timeout/1,
	 t_connect_bad/1,
	 t_recv_timeout/1, t_recv_eof/1,
	 t_shutdown_write/1, t_shutdown_both/1, t_shutdown_error/1,
	 t_shutdown_async/1,
	 t_fdopen/1, t_fdconnect/1]).

-export([getsockfd/0,closesockfd/1]).

suite() -> []. %% {ct_hooks,[ts_install_cth]}].

all() ->
    [{group, t_accept}, {group, t_connect}, {group, t_recv},
     t_shutdown_write, t_shutdown_both, t_shutdown_error,
     t_shutdown_async, t_fdopen, t_fdconnect].

groups() ->
    [{t_accept, [shuffle], [t_accept_timeout]},
     {t_connect, [shuffle], [t_connect_bad]},
     {t_recv, [shuffle], [t_recv_timeout, t_recv_eof]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_,_Config) ->
    ok.

init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(test_server:seconds(60)),
    [{watchdog, Dog}|Config].
end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).

%%% afunix:accept/1,2


t_accept_timeout(doc) -> "Test that afunix:accept/2 (with timeout) works.";
t_accept_timeout(suite) -> [];
t_accept_timeout(Config) when is_list(Config) ->
    SocketName = socket_name(Config, "socket_t_accept_timeout"),
    ?line {ok, L} = afunix:listen(SocketName, []),
    ?line timeout({gen_tcp, accept, [L, 200]}, 0.2, 1.0).

t_connect_bad(doc) ->
    ["Test that afunix:connect/3 handles non-existings hosts, and other ",
     "invalid things."];
t_connect_bad(suite) -> [];
t_connect_bad(Config) when is_list(Config) ->
    SocketName = socket_name(Config,"socket_t_connect_bad"),
    ?line {error, Reason1} = afunix:connect(SocketName, []),
    ?line io:format("Error for connection attempt to name not in use: ~p",
		    [Reason1]),
    ok.

%%% afunix:recv/X

t_recv_timeout(doc) -> "Test that afunix:recv/3 (with timeout works).";
t_recv_timeout(suite) -> [];
t_recv_timeout(Config) when is_list(Config) ->
    SocketName = socket_name(Config, "socket_t_recv_timeout"),
    ?line {ok, L} = afunix:listen(SocketName, []),
    ?line {ok, Client} = afunix:connect(SocketName, [{active, false}]),
    ?line {ok, _A} = afunix:accept(L),
    ?line timeout({afunix, recv, [Client, 0, 200]}, 0.2, 5.0).

t_recv_eof(doc) -> "Test that end of file on a socket is reported correctly.";
t_recv_eof(suite) -> [];
t_recv_eof(Config) when is_list(Config) ->
    SocketName = socket_name(Config, "socket_t_recv_timeout"),
    ?line {ok, L} = afunix:listen(SocketName, []),
    ?line {ok, Client} = afunix:connect(SocketName, [{active, false}]),
    ?line {ok, A} = afunix:accept(L),
    ?line ok = afunix:close(A),
    ?line {error, closed} = afunix:recv(Client, 0),
    ok.

%%% afunix:shutdown/2

t_shutdown_write(Config) when is_list(Config) ->
    SocketName = socket_name(Config,"socket_t_shutdown_write"),
    ?line {ok, L} = afunix:listen(SocketName, []),
    ?line {ok, Client} = afunix:connect(SocketName, [{active, false}]),
    ?line {ok, A} = afunix:accept(L),
    ?line ok = afunix:shutdown(A, write),
    ?line {error, closed} = afunix:recv(Client, 0),
    ok.

t_shutdown_both(Config) when is_list(Config) ->
    SocketName = socket_name(Config,"socket_t_shutdown_both"),
    ?line {ok, L} = afunix:listen(SocketName, []),
    ?line {ok, Client} = afunix:connect(SocketName, [{active, false}]),
    ?line {ok, A} = afunix:accept(L),
    ?line ok = afunix:shutdown(A, read_write),
    ?line {error, closed} = afunix:recv(Client, 0),
    ok.

t_shutdown_error(Config) when is_list(Config) ->
    SocketName = socket_name(Config,"socket_t_shutdown_error"),
    ?line {ok, L} = afunix:listen(SocketName, []),
    ?line {error, enotconn} = afunix:shutdown(L, read_write),
    ?line ok = afunix:close(L),
    ?line {error, closed} = afunix:shutdown(L, read_write),
    ok.

t_shutdown_async(Config) when is_list(Config) ->
    SocketName = socket_name(Config,"socket_t_shutdown_async"),
    ?line {OS, _} = os:type(),
    ?line {ok, L} = afunix:listen(SocketName, [{sndbuf, 4096}]),
    ?line {ok, Client} = afunix:connect(SocketName,
					[{recbuf, 4096},
					 {active, false}]),
    ?line {ok, S} = afunix:accept(L),
    ?line PayloadSize = 1024 * 1024,
    ?line Payload = lists:duplicate(PayloadSize, $.),
    ?line ok = afunix:send(S, Payload),
    ?line case erlang:port_info(S, queue_size) of
	      {queue_size, N} when N > 0 -> ok;
	      {queue_size, 0} when OS =:= win32 -> ok;
	      {queue_size, 0} = T -> ?t:fail({unexpected, T})
	  end,

    ?line ok = afunix:shutdown(S, write),
    ?line {ok, Buf} = afunix:recv(Client, PayloadSize),
    ?line {error, closed} = afunix:recv(Client, 0),
    ?line case length(Buf) of
	      PayloadSize -> ok;
	      Sz -> ?t:fail({payload_size,
			     {expected, PayloadSize},
			     {received, Sz}})
	  end.


%%% afunix:fdopen/2

t_fdopen(Config) when is_list(Config) ->
    ?line Question = "Aaaa... Long time ago in a small town in Germany,",
    ?line Question1 = list_to_binary(Question),
    ?line Question2 = [<<"Aaaa">>, "... ", $L, <<>>, $o, "ng time ago ",
                       ["in ", [], <<"a small town">>, [" in Germany,", <<>>]]],
    ?line Question1 = iolist_to_binary(Question2),
    ?line Answer = "there was a shoemaker, Schumacher was his name.",
    SocketName = socket_name(Config,"socket_t_fdopen"),
    ?line {ok, L} = afunix:listen(SocketName, [{active, false}]),
    ?line {ok, Client} = afunix:connect(SocketName, [{active, false}]),
    ?line {ok, A} = afunix:accept(L),
    ?line {ok, FD} = prim_inet:getfd(A),
    ?line {ok, Server} = afunix:fdopen(FD, []),
    ?line ok = afunix:send(Client, Question),
    ?line {ok, Question} = afunix:recv(Server, length(Question), 2000),
    ?line ok = afunix:send(Client, Question1),
    ?line {ok, Question} = afunix:recv(Server, length(Question), 2000),
    ?line ok = afunix:send(Client, Question2),
    ?line {ok, Question} = afunix:recv(Server, length(Question), 2000),
    ?line ok = afunix:send(Server, Answer),
    ?line {ok, Answer} = afunix:recv(Client, length(Answer), 2000),
    ?line ok = afunix:close(Client),
    ?line {error,closed} = afunix:recv(A, 1, 2000),
    ?line ok = afunix:close(Server),
    ?line ok = afunix:close(A),
    ?line ok = afunix:close(L),
    ok.

t_fdconnect(Config) when is_list(Config) ->
    Question = "Aaaa... Long time ago in a small town in Germany,",
    Question1 = list_to_binary(Question),
    Question2 = [<<"Aaaa">>, "... ", $L, <<>>, $o, "ng time ago ",
                       ["in ", [], <<"a small town">>, [" in Germany,", <<>>]]],
    Question1 = iolist_to_binary(Question2),
    Answer = "there was a shoemaker, Schumacher was his name.",
    Path = "../../../../test/priv",
    Lib = "afunix_api_SUITE",
    ok = erlang:load_nif(filename:join(Path,Lib), []),
    SocketName = socket_name(Config,"socket_t_fdopen"),
    {ok, L} = afunix:listen(SocketName, [{active, false}]),
    FD = afunix_api_SUITE:getsockfd(),
    {ok, Client} = afunix:connect(SocketName, [{fd,FD},{port,20002},
					       {active,false}]),
    {ok, Server} = afunix:accept(L),
    ok = afunix:send(Client, Question),
    {ok, Question} = afunix:recv(Server, length(Question), 2000),
    ok = afunix:send(Client, Question1),
    {ok, Question} = afunix:recv(Server, length(Question), 2000),
    ok = afunix:send(Client, Question2),
    {ok, Question} = afunix:recv(Server, length(Question), 2000),
    ok = afunix:send(Server, Answer),
    {ok, Answer} = afunix:recv(Client, length(Answer), 2000),
    ok = afunix:close(Client),
    FD = afunix_api_SUITE:closesockfd(FD),
    {error,closed} = afunix:recv(Server, 1, 2000),
    ok = afunix:close(Server),
    ok = afunix:close(L),
    ok.

%%% Utilities

%% Calls M:F/length(A), which should return a timeout error, and complete
%% within the given time.

socket_name(Config, Name) ->
    Path = ?config(data_dir, Config),
    SocketName = filename:join(Path,Name),
    file:delete(SocketName),
    SocketName.

timeout({M,F,A}, Lower, Upper) ->
    case test_server:timecall(M, F, A) of
	{Time, Result} when Time < Lower ->
	    test_server:fail({too_short_time, Time, Result});
	{Time, Result} when Time > Upper ->
	    test_server:fail({too_long_time, Time, Result});
	{_, {error, timeout}} ->
	    ok;
	{_, Result} ->
	    test_server:fail({unexpected_result, Result})
    end.

getsockfd() -> undefined.
closesockfd(_FD) -> undefined.
