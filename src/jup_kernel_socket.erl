-module(jup_kernel_socket).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/5,
        send/3
       ]).


-export([
         init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3
        ]).


-record(state, {
          socket,
          bind,
          key,
          ref,
          receiver
         }).


start_link(Name, PortName, Kind, Port, ConnData) ->
    gen_server:start_link(?JUP_VIA(Name, PortName), ?MODULE,
                          [Name, PortName, Kind, Port, ConnData],
                          []
                         ).


send(Name, PortName, Msg = #jup_msg{}) ->
    gen_server:call(?JUP_VIA(Name, PortName), {send, Msg}).


init([Name, PortName, Kind, Port, ConnData]) ->
    Identity = string:join([atom_to_list(Name), atom_to_list(PortName)], "-"),
    {ok, Socket} = chumak:socket(Kind, Identity),
    {ok, Bind} = chumak:bind(
                   Socket,
                   ConnData#jup_conn_data.transport,
                   binary_to_list(ConnData#jup_conn_data.ip),
                   Port
                  ),

    link(Bind),

    Ref = make_ref(),
    % Self = self(),

    {ok, #state{
            socket=Socket,
            ref=Ref,
            receiver=spawn_link(
                       fun () ->
                               do_receive_multipart(
                                 {Name, PortName}, Socket,
                                 ConnData#jup_conn_data.signature_key
                                )
                       end
                      ),
            key=ConnData#jup_conn_data.signature_key
           }
    }.


handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).


handle_call({send, Msg = #jup_msg{}}, _From, State) ->
    Encoded = jup_msg:encode(Msg, State#state.key),
    chumak:send_multipart(State#state.socket, Encoded),
    {reply, ok, State}.


code_change(_OldVsn, State, _Extra) ->
    State.


terminate(_Reason, _State) ->
    ok.


do_receive_multipart({Name, PortName}, Socket, SignatureKey) ->
    {ok, Mp} = chumak:recv_multipart(Socket),

    Decoded = jup_msg:decode(Mp, SignatureKey),
    MsgType = jup_msg:msg_type(Decoded),

    lager:debug("Received message of type ~p:~n~p",
                [MsgType, lager:pr(Decoded, ?MODULE)]
               ),

    jup_kernel_protocol:process_message(
      Name, PortName, MsgType, Decoded
     ),

    % Callback(Name, PortName, MsgType, Decoded),
    do_receive_multipart({Name, PortName}, Socket, SignatureKey).


