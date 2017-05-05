-module(ierl_connection_file).

-export([
    parse/1
]).

parse(Filename) ->
    {ok, Raw} = file:read_file(Filename),
    JsonData = jsx:decode(Raw),

    StdInPort = proplists:get_value(<<"stdin_port">>, JsonData),
    IP = proplists:get_value(<<"ip">>, JsonData),
    ControlPort = proplists:get_value(<<"control_port">>, JsonData),
    HbPort = proplists:get_value(<<"hb_port">>, JsonData),
    SignatureScheme = proplists:get_value(<<"signature_scheme">>, JsonData),
    Key = proplists:get_value(<<"key">>, JsonData),
    ShellPort = proplists:get_value(<<"shell_port">>, JsonData),
    Transport = proplists:get_value(<<"transport">>, JsonData),
    IOPubPort = proplists:get_value(<<"iopub_port">>, JsonData),
    {ok,
     StdInPort, IP, ControlPort, HbPort, SignatureScheme, Key, ShellPort,
     Transport, IOPubPort}.
