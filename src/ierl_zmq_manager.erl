%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, <COMPANY>
%%% @doc Set up all the ZMQ sockets and bindings, also spawns the servers
%%%      that handle the messaging. Once this is done it goes into an
%%%      infinite empty loop to keep IErlang process alive.
%%%
%%% @end
%%% Created : 03. Apr 2014 11:51
%%%-------------------------------------------------------------------
-module (ierl_zmq_manager).
-author("Robbie Lynch").
-export([run/1]).

-include("./records.hrl").

%% @doc Function that creates and binds all zmq sockets. Starts the heartbeat, shell and control servers.
run(#ierl_connection_file{} = ConnData)->
    %% Create Sockets that will be used to communicate with IPython
    {ok, HeartbeatSocket} = chumak:socket(rep, "heartbeat"),
    {ok, ControlSocket} = chumak:socket(router, "control"),
    {ok, StdinSocket} = chumak:socket(router, "stdin"),
    {ok, ShellSocket} = chumak:socket(router, "shell"),
    {ok, IOPubSocket} = chumak:socket(pub, "pub"),

    Bind = fun (Socket, Port) ->
                   {ok, _Pid} = chumak:bind(
                                  Socket,
                                  ConnData#ierl_connection_file.transport,
                                  binary_to_list(ConnData#ierl_connection_file.ip),
                                  Port
                                 )
           end,

    Bind(HeartbeatSocket, ConnData#ierl_connection_file.heartbeat_port),
    Bind(ControlSocket, ConnData#ierl_connection_file.control_port),
    Bind(StdinSocket, ConnData#ierl_connection_file.stdin_port),
    Bind(ShellSocket, ConnData#ierl_connection_file.shell_port),
    Bind(IOPubSocket, ConnData#ierl_connection_file.iopub_port),

    % Start the heartbeat server
    spawn_link(ierl_heartbeat_server, start, [HeartbeatSocket]),
    % Start the Shell server
    spawn_link(ierl_shell_server, start, [ShellSocket, IOPubSocket, ConnData]),
    % Start the Control server
    spawn_link(ierl_control_server, start, [ControlSocket]),

    %% Constantly listen and reply to messages
    loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket).

%% Function to listen and respond on all sockets.
%% Constant loop keeps the kernel alive.
loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket)->
    %% Keep listening and responding
    timer:sleep(5),
    loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket).
