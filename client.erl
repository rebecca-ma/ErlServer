-module(client).
-export([connect/2, send_message/1, close/0]).

connect(Port, IpAddr) ->
    {ok, Socket} = gen_tcp:connect(IpAddr, Port, [binary, {active, true}]),
    Pid = spawn(fun() -> main(Socket) end),
    register(main, Pid),
    gen_tcp:controlling_process(Socket, Pid).

send_message(Message) ->
    main!{send, Message}.

close() ->
    main!stop.

main(Socket) ->
    receive
        stop ->
            gen_tcp:close(Socket),
            io:format("Connection closed~n", []);
        {send, Message} ->
            gen_tcp:send(Socket, Message),
            main(Socket);
        {tcp, Socket, Message} ->
            io:format("Received message ~p~n", [Message]),
            main(Socket);
        {tcp_closed, _} ->
            io:format("Server closed connection, exiting~n", []),
            gen_tcp:close(Socket)
    end.

