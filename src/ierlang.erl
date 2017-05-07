%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc This modules defines functions used to starts all the
%%%      IErlang servers, to handle messages, and to parse the json
%%%      configuration file.
%%%
%%% @end
%%% Created : 31. Mar 2014 10:02
%%%-------------------------------------------------------------------
-module (ierlang).
-export ([main/1]).

-include("./records.hrl").

%% JSON FILE CONTENTS
%% Receives a list of arguments from IPython.
%% The list contains the absolute path to the kernel.json file.
%% The kernel.json file contains:
%%      stdin_port: 52248,
%%      ip: "127.0.0.1",
%%      control_port: 52249,
%%      hb_port: 52250,
%%      signature_scheme: "hmac-sha256",
%%      key: "",
%%      shell_port: 52246,
%%      transport: "tcp",
%%      iopub_port: 52247

main([JsonFile]) ->
    {ok, _Deps} = application:ensure_all_started(ierlang),

    lager:info("Starting Erlang kernel with connection file ~s", [JsonFile]),
    %% Read json file
    ConnData = #ierl_connection_file{} = ierl_connection_file:parse(JsonFile),
    ierl_zmq_manager:run(ConnData).
