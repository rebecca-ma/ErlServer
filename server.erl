-module(server).
-export([launch/1, stop/0, print_slaves/0]).

launch(Port) ->
    Response = gen_tcp:listen(Port, [{active, true}, binary]),
    Listener_Pid = spawn(fun() -> listen(Response) end),
    io:format("Launched Listener~n", []),
    register(master, spawn(fun() -> master([], Listener_Pid) end)),
    io:format("Launched and Registered Master~n", []).

stop() ->
    master!stop.

print_slaves() ->
    master!print_slaves.

%-----------------

master(Slaves, Listener_Pid) ->
    process_flag(trap_exit, true),
    link(Listener_Pid),
    receive
        stop ->
            io:format("Master received exit command~n", []),
            kill_slaves(Slaves);
        print_slaves ->
            io:format("~p~n", [Slaves]),
            master(Slaves, Listener_Pid);
        {'EXIT', Listener_Pid, Data} ->
            io:format("Listener exited unexpectedly~n", []),
            kill_slaves(Slaves);
        {'EXIT', Pid, Data} ->
            io:format("Process ~p exited~n", [Pid]),
            master(lists:filter(fun (Sid) -> Sid /= Pid end, Slaves),
                    Listener_Pid);
        {new, Pid} ->
            io:format("New slave with Pid ~p~n", [Pid]),
            link(Pid),
            master([Pid | Slaves], Listener_Pid)
    end.

listen({ok, Socket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(Socket),
    Pid = spawn(fun() -> handle(AcceptSocket) end),
    gen_tcp:controlling_process(AcceptSocket, Pid),
    master!{new, Pid},
    listen({ok, Socket});
listen({error, Error}) ->
    io:format("Could not listen: ~p~n", [Error]).

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

