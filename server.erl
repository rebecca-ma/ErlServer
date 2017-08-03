-module(server).
-export([launch/1, stop/0, print_slaves/0]).

launch(Port) ->
    Response = gen_tcp:listen(Port, [{active, true}, binary]),
    check_listen_socket(Response).

stop() ->
    master!stop.

print_slaves() ->
    master!print_slaves.

%-----------------

check_listen_socket({ok, Socket}) ->
    Listener_Pid = spawn(fun() -> listen(Socket) end),
    io:format("Launched Listener~n", []),
    register(master, spawn(fun() -> master([], Listener_Pid, Socket) end)),
    io:format("Launched and Registered Master~n", []);
check_listen_socket({error, Reason}) ->
    io:format("Listen Socket couldn't be opened: ~p~n", [Reason]).

check_accept_socket({ok, AcceptSocket}, ListenSocket) ->
    Pid = spawn(fun() -> handle(AcceptSocket) end),
    gen_tcp:controlling_process(AcceptSocket, Pid),
    master!{new, Pid},
    listen(ListenSocket);
check_accept_socket({error, closed}, _) ->
    io:format("Listen Socket has closed~n", []);
check_accept_socket({error, Reason}, ListenSocket) ->
    io:format("Could not accept an incoming connection: ~p~n", [Reason]),
    listen(ListenSocket).

handle(Socket) ->
    receive
        stop ->
            io:format("Slave ~p dying~n", [self()]),
            gen_tcp:close(Socket);
        {tcp, Socket, Message} ->
            io:format("Received message: ~p~n", [Message]),
            gen_tcp:send(Socket, app:response(Message)),
            handle(Socket);
        {tcp_closed, Socket} ->
            io:format("Client disconnected~n", []),
            gen_tcp:close(Socket)
    end.

kill_slaves([]) ->
    io:format("All slaves dead~n", []);
kill_slaves([Slave | Slaves]) ->
    io:format("Killing slave ~p~n", [Slave]),
    Slave!stop,
    kill_slaves(Slaves).

listen(Socket) ->
    Response = gen_tcp:accept(Socket),
    check_accept_socket(Response, Socket).

master(Slaves, Listener_Pid, ListenSocket) ->
    process_flag(trap_exit, true),
    link(Listener_Pid),
    receive
        stop ->
            io:format("Master received exit command~n", []),
            gen_tcp:close(ListenSocket),
            kill_slaves(Slaves);
        print_slaves ->
            io:format("~p~n", [Slaves]),
            master(Slaves, Listener_Pid, ListenSocket);
        {'EXIT', Listener_Pid, Data} ->
            io:format("Listener exited unexpectedly: ~p~n", [Data]),
            kill_slaves(Slaves);
        {'EXIT', Pid, Data} ->
            io:format("Process ~p exited: ~p~n", [Pid, Data]),
            master(lists:filter(fun (Sid) -> Sid /= Pid end, Slaves),
                    Listener_Pid, ListenSocket);
        {new, Pid} ->
            io:format("New slave with Pid ~p~n", [Pid]),
            link(Pid),
            master([Pid | Slaves], Listener_Pid, ListenSocket)
    end.

