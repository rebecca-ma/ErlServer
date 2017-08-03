-module(app).
-export([response/1]).

response(Message) ->
    string:concat("Message received: ", Message).
