-module(jup_kernel_protocol).

-include("internal.hrl").

-export([
         process_message/4
        ]).



process_message(Name, Port, MsgType, Msg) ->
    do_iopub(Name, #{ status => busy }),
    % TODO: Catch and send error?
    case do_process(Name, Port, MsgType, Msg) of
        {Status, Result} ->
            do_reply(Name, Port, Status, Result, Msg);
        noreply ->
            ok
    end,
    do_iopub(Name, #{ status => idle }).


do_reply(Name, Port, Status, NewMsg, Msg) ->
    NewMsg1 = case NewMsg of
                  #jup_msg{} -> NewMsg;
                  _ -> #jup_msg{content=NewMsg}
              end,

    Reply = jup_msg:add_headers(
              NewMsg1, Msg,
              to_reply_type(jup_msg:msg_type(Msg))
             ),

    jup_kernel_socket:send(Name, Port, Reply#{ status => Status }).


do_iopub(Name, Msg) ->
    lager:info("[IOPUB ~p]: ~p", [Name, Msg]).


to_reply_type(MsgType) ->
    binary:replace(MsgType, <<"request">>, <<"reply">>).


do_process(Name, shell, MsgType, Msg) ->
    {ok, Version} = file:read_file(
                      filename:join(
                        [
                         code:root_dir(),
                         "releases",
                         erlang:system_info(otp_release),
                         "OTP_VERSION"
                        ]
                       )
                     ),

    %    Build the proplist to be converted to json
    Content =
    #{
      protocol_version => <<"5.1">>,
      implementation => <<"IErlang">>,
      implementation_version => <<"0.2">>,
      % TODO: Show which node we are running against
      banner => <<"Erlang kernel">>,
      language_info => #{
        name => erlang,
        version => Version,
        file_extension => <<".erl">>
       }
     },

    Content;

do_process(Name, Source, MsgType, Msg) ->
    lager:debug("Not implemented on ~s: ~p:~s~n~p", [Name, Source, MsgType,
                                                     lager:pr(Msg, ?MODULE)
                                                    ]
               ),

    noreply.
