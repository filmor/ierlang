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

%% @doc Function that creates and binds all zmq sockets. Starts the heartbeat, shell and control servers.
run([{hbport, HbPort}, {shellport, ShellPort}, {controlport, ControlPort}, {iopubport, IOPubPort}, {stdinport, StdInPort}, {ip, IP}, {transport, Transport}])->

    application:start(chumak),

    % TODO: to existing atom
    Transport1 = list_to_atom(binary_to_list(Transport)),
    IP1 = binary_to_list(IP),

    %% Create Sockets that will be used to communicate with IPython
    {ok, HeartbeatSocket} = chumak:socket(rep, "heartbeat"),
    {ok, ControlSocket} = chumak:socket(router, "control"),
    {ok, StdinSocket} = chumak:socket(router, "stdin"),
    {ok, ShellSocket} = chumak:socket(router, "shell"),
    {ok, IOPubSocket} = chumak:socket(pub, "pub"),

    Bind = fun (Socket, Port) ->
                   {ok, _Pid} = chumak:bind(Socket, Transport1, IP1, Port)
           end,

    Bind(HeartbeatSocket, HbPort),
    Bind(ControlSocket, ControlPort),
    Bind(StdinSocket, StdInPort),
    Bind(ShellSocket, ShellPort),
    Bind(IOPubSocket, IOPubPort),


    % Start the heartbeat server
    spawn(ierl_heartbeat_server, start, [HeartbeatSocket]),
    % Start the Shell server
    spawn(ierl_shell_server, start, [ShellSocket, IOPubSocket]),
    % Start the Control server
    spawn(ierl_control_server, start, [ControlSocket]),

    %% Constantly listen and reply to messages
    loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket).

%% Function to listen and respond on all sockets.
%% Constant loop keeps the kernel alive.
loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket)->
    %% Keep listening and responding
    timer:sleep(5),
    loop(HeartbeatSocket, ControlSocket, StdinSocket, ShellSocket, IOPubSocket).
