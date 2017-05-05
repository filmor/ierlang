%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc The heartbeat sever keeps IPython alive by constantly
%%%      listening for messages on the given socket and replying
%%%      with a ping message.
%%% @end
%%% Created : 03. Apr 2014 11:51
%%%-------------------------------------------------------------------
-module(ierl_heartbeat_server).
-author("Robbie Lynch").
-export([start/1]).


%%% @doc Starts the heartbeat server
start(HeartbeatSocket) ->
    loop(HeartbeatSocket).

loop(HeartbeatSocket) ->
   heartbeat_listener(HeartbeatSocket),
   loop(HeartbeatSocket).

%%% @doc Heartbeat - this keeps IPython alive
%%%      by listening and replying to ping messages
heartbeat_listener(HeartbeatSocket)->
    {ok, Msg} = chumak:recv(HeartbeatSocket),
    ok = chumak:send(HeartbeatSocket, Msg).
