-module(ierl_connection_file).

-include("internal.hrl").

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

    #jup_conn_data{
       transport = binary_to_existing_atom(Transport, utf8),
       ip = IP,

       control_port = ControlPort,
       heartbeat_port = HbPort,
       shell_port = ShellPort,
       iopub_port = IOPubPort,
       stdin_port = StdInPort,

       signature_key = {parse_signature_scheme(SignatureScheme), Key}
      }.


parse_signature_scheme(<<"hmac-sha256">>) ->
    sha256;

parse_signature_scheme(Algo) ->
    error({unknown_signature_scheme, Algo}).
