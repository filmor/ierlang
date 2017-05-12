%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc The Shell Server handles the most important messages received
%%%      from IPython.
%%%
%%% @end
%%% Created : 03. Apr 2014 11:51
%%%-------------------------------------------------------------------
-module (ierl_shell_server).
-author("Robbie Lynch").
-export ([start/3]).

-include("internal.hrl").

-record(state,
        {
         shell_socket,
         iopub_socket,
         exec_count,
         key
        }).

%%% @doc Starts the shell server
start(ShellSocket, IOPubSocket, ConnData) ->
    State = #state{
               shell_socket=ShellSocket,
               iopub_socket=IOPubSocket,
               exec_count=0,
               key=ConnData#jup_conn_data.signature_key
              },

    loop(State).

loop(State) ->
   State1 = shell_listener(State),
   loop(State1).


%%% @doc Listens for messages on the Shell Socket, parses and
%%%      acts upon the message contents, then replies to IPython
shell_listener(State)->
    {ok, Mp} = chumak:recv_multipart(State#state.shell_socket),

    Decoded = jup_msg:decode(Mp, State#state.key),
    lager:debug("Received message ~p", [lager:pr(Decoded, ?MODULE)]),

    MsgType = jup_msg:msg_type(Decoded),
    State1 = process_message(MsgType, Decoded, State),
    State1.


process_message(<<"kernel_info_request">>, Msg, State) ->
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

    reply(Content, Msg, shell, State),
    State;

process_message(<<"is_complete_request">>, Msg, State) ->
    % TODO Check for completeness
    reply(#{ status => complete }, Msg, shell, State),
    State;

process_message(<<"complete_request">>, Msg, State) ->
    reply(#{ status => <<"error">> }, Msg, shell, State),
    State;

process_message(<<"execute_request">>, Msg, State) ->
    reply(#{ status => <<"error">> }, Msg, shell, State),
    State#state{exec_count=State#state.exec_count + 1};

process_message(_Unknown, Msg, State) ->
    lager:warning("Received unimplemented message: ~s", [_Unknown]),
    reply(#{ status => <<"error">> }, Msg, shell, State),
    State.


reply(NewMsg, Msg, Socket, State) ->
    NewMsg1 = case NewMsg of
                  #jup_msg{} -> NewMsg;
                  _ -> #jup_msg{content=NewMsg}
              end,

    Reply = jup_msg:add_headers(
              NewMsg1, Msg,
              to_reply_type(jup_msg:msg_type(Msg))
             ),

    do_send(Reply, Socket, State).


do_send(Msg, Socket, State) ->
    Socket1 = case Socket of
                  shell ->
                      State#state.shell_socket;
                  iopub ->
                      State#state.iopub_socket
              end,

    Encoded = jup_msg:encode(Msg, State#state.key),
    lager:debug("Encoded message: ~p", [Encoded]),
    chumak:send_multipart(Socket1, Encoded).


to_reply_type(MsgType) ->
    binary:replace(MsgType, <<"request">>, <<"reply">>).
