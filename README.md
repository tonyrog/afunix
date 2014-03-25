The ultimate afunix documentation
=================================

afunix is an api to unix domain sockets. The afunix is a "plugin" 
to the inet/gen_tcp. The api is binary compatible with the gen_tcp interface.
The afunix is not available on the windows platform, hence the unix part
of afunix.

To connect to a unix domain socket you do

    afunix:connect(Name, Options) -> {ok,socket()} | {error,posix_error()}

To start a unix domain socket server you do

    afunix:listen(Name, Options) ->  {ok,socket()} | {error,posix_error()}

From there on you can use the "normal" inet and gen_tcp interface.

Example:

Shell 1:

    1> {ok,L} = afunix:listen("/tmp/foo", []).
    {ok,#Port<0.2229>}
    2> {ok,S} = gen_tcp:accept(L).

Shell 2:

    1> {ok,S} = afunix:connect("/tmp/foo", []).
    {ok,#Port<0.2229>}
    2> afunix:send(S, "Hello").
    ok
    3> gen_tcp:send(S, "Hej").        
    ok

Shell 1:

    {ok,#Port<0.2244>}
    3> flush().
    Shell got {tcp,#Port<0.2244>,"Hello"}
    Shell got {tcp,#Port<0.2244>,"Hej"}
    ok


It is also possible to use the corresponding afunix module functions,
the following functions are avaiable

    afunix:connect(Name::string(),Options::[connect_option()]).
    afunix:connect(Name::string(),Options::[connect_option()],Tmo::timeout()).
    afunix:listen(Name::string(), Options::[listen_option()]).
    afunix:accept(ListenSocket::socket())
    afunix:accept(ListenSocket::socket(), Tmo::timeout())
    afunix:close(Socket::socket()).
    afunix:send(Socket::socket(), Packet::iodata()).
    afunix:send(Socket::socket(), Packet::iodata(), Options).
    afunix:recv(Socket::socket(), Length::non_neg_integer()).
    afunix:recv(Socket::socket(), Length::non_neg_integer(),Tmo::timeout()).
    afunix:unrecv(Socket::socket(), Data::iodata()).
    afunix:shutdown(Socket::socket(), read|write|read_write).
    afunix:controlling_process(Socket::socket(), Pid::pid()).
    afunix:fdopen(Fd::integer(), Options).

Special api functions

    afunix:get_peercred(Socket::socket())

get_peercred return {ok,Uid}, the effective user id of the socket peer,
or {error,Reason}.

    afunix:get_peerpid(Socket::socket())

get_peerpid return {ok,Pid}, the process id of the socket peer,
or {error,Reason}.
