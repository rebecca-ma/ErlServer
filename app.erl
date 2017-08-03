-module(app).
-export([response/1]).

response(Message) ->
    string:concat("I heard you say ", Message).
