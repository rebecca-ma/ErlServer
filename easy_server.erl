-module(easy_server).
-export([launch/1, stop/0]).

launch(Port) ->
    register(master, spawn(fun() -> master([]) end)),
    Response = gen_tcp:listen(Port, [{active,true}, binary]),
    register(listener, spawn(fun() -> listen(Response) end)).

stop() ->
    master!stop.

master(Slaves) ->
    process_flag(trap_exit, true),
    receive
        stop ->
            kill_slaves(Slaves),
            io:format("Master is exiting~n", []);
        {new, Pid} ->
            io:format("Registering new slave with PID ~p~n", [Pid]),
            link(Pid),
            master([Pid | Slaves]);
        {"EXIT", Pid, Data} ->
            io:format("Slave ~p exited~n", [Pid]),
            master(lists:filter(fun (Sid) -> Sid /= Pid end, Slaves));
        {Pid, Message} ->
            io:format("Received Message: ~p~n", [Message]),
            master(Slaves)
    end.

listen({ok, ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    Pid = spawn(easy_server, handler, [AcceptSocket]), 
    gen_tcp:controlling_process(AcceptSocket, Pid),
    master!{new, Pid},
    listen({ok, ListenSocket});
listen({error, Error}) ->
    io:format("Could not listen on port 8091: ~p~n", [Error]).

handler(Socket) ->
    receive
        stop ->
            io:format("Slave dying~n", []),
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            io:format("Received message: ~p~n", [Msg]),
            gen_tcp:close(Socket)
    end.

kill_slaves([]) ->
    io:format("All Slaves killed~n", []);
kill_slaves([Slave | Slaves]) ->
    Slave!stop,
    kill_slaves(Slaves).
