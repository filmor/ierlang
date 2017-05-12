-module(jup_kernel_backend).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/2
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
          name,
          backend,
          backend_state
         }).


-spec start_link(Name :: atom(), Backend :: module()) -> {ok, pid()}.
start_link(Name, Backend) ->
    gen_server:start_link(?JUP_VIA(Name, backend), ?MODULE, [Name, Backend], []).


init([Name, Backend]) ->
    {ok, #state{
            name=Name,
            backend=Backend,
            backend_state=undefined % Backend:init()
           }
    }.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).

handle_call(_Call, _From, _State) ->
    error({invalid_call, _Call}).

code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, _State) ->
    ok.
