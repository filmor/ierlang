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

-include("internal.hrl").

main([JsonFile]) ->
    % TODO: Add proper command-line handling.

    {ok, _Deps} = application:ensure_all_started(ierlang),
    lager:set_loglevel(lager_console_backend, debug),

    lager:info("Starting Erlang kernel with connection file ~s", [JsonFile]),
    %% Read json file
    ConnData = #jup_conn_data{} = ierl_connection_file:parse(JsonFile),

    {ok, Pid} = jup_kernel_sup:start_link(
                  ierlang, ConnData, jup_kernel_backend_stub
                 ),

    % TODO: Start the supervisor via application, set_env to set the options?
    MonitorRef = monitor(process, Pid),
    receive
        {'DOWN', MonitorRef, process, _, _} ->
            lager:info("Kernel supervisor is down, stopping.")
    end.
