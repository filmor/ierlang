-module(jup_kernel_iopub_srv).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/2,
        send/2
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
          key
         }).


start_link(Name, ConnData) ->
    gen_server:start_link(?JUP_VIA(Name, iopub), ?MODULE,
                          [Name, ConnData], []
                         ).


send(Name, Msg = #jup_msg{}) ->
    gproc:send(?JUP_NAME(Name, iopub), Msg).


init([Name, ConnData]) ->
    Identity = string:join([atom_to_list(Name), "iopub"], "-"),
    {ok, Socket} = chumak:socket(pub, Identity),
    {ok, Bind} = chumak:bind(
                   Socket,
                   ConnData#jup_conn_data.transport,
                   binary_to_list(ConnData#jup_conn_data.ip),
                   ConnData#jup_conn_data.iopub_port
                  ),

    link(Bind),

    % Self = self(),

    {ok, #state{
            socket=Socket,
            bind=Bind,
            key=ConnData#jup_conn_data.signature_key
           }
    }.


handle_info(#jup_msg{} = Msg, State) ->
    Encoded = jup_msg:encode(Msg, State#state.key),
    chumak:send_multipart(State#state.socket, Encoded),
    {noreply, State};

handle_info(_Msg, State) ->
    lager:debug("Unrecognized message: ~p", [_Msg]),
    {noreply, State}.

handle_call(_Call, _From, _State) ->
    error({invalid_call, _Call}).

handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).

code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, _State) ->
    ok.
