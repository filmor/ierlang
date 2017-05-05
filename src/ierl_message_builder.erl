%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc Module that contains the functions to create replies to
%%%      IPython messages.
%%%
%%% @end
%%% Created : 03. Apr 2014 10:51
%%%-------------------------------------------------------------------
-module(ierl_message_builder).
-author("Robbie Lynch").
-export([generate_content_reply/2, generate_content_reply/1,
         create_metadata/0, generate_header_reply/3]).
-define(USERNAME, ierlang_kernel).
-define(IDLE_STATUS, idle).
-define(BUSY_STATUS, busy).
-define(STARTING_STATUS, starting).
-define(OK_STATUS, ok).
-define(ERROR_STATUS, error).

%% @spec generate_header_reply(list(), list(), list()) -> list()
%% @doc Creates the header for the message being sent to IPython
generate_header_reply(Session, MessageType, Date)->
  HeaderPropList = [
    {date, Date},
    {username, ?USERNAME},
    {session, Session},
    {msg_id, list_to_binary(uuid:uuid_to_string(uuid:get_v4()))},
    {msg_type, MessageType}
  ],
  Header = jsx:encode(HeaderPropList),
  Header.

%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the busy status sent over the
%%      iopub socket.
generate_content_reply(busy)->
  %%Should be sent before the execution of the code
  Content = [{execution_state, ?BUSY_STATUS}],
  ContentJson = jsx:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the idle status sent over the
%%      iopub socket.
generate_content_reply(idle) ->
  Content = [{execution_state, ?IDLE_STATUS}],
  ContentJson = jsx:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the starting status sent over the
%%      iopub socket.
generate_content_reply(starting)->
  Content = [{execution_state, ?STARTING_STATUS}],
  ContentJson = jsx:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom()) -> list()
%% @doc Creates the content reply for the kernel_info_reply sent over the
%%      shell socket.
generate_content_reply(kernel_info_reply)->
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
      language_info => #{
        name => erlang,
        version => Version,
        file_extension => <<".erl">>
       }
     },
    %    Build the Json Reply
    ReplyJson = jsx:encode(Content),
    ReplyJson.

%% @doc Creates the content reply for a successful execute_reply
%%      sent over the shell socket.
generate_content_reply(execute_reply, {"ok", ExecutionCount, _UserVars, _UserExprs})->
  Content = [
    {status, ?OK_STATUS},
    {execution_count, ExecutionCount},
    {payload, []},
    {user_variables, {}},
    {user_expressions, {}}
  ],
  ContentJson = jsx:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for an unsuccessful execute_reply
%%      sent over the shell socket.
generate_content_reply(execute_reply_error, {"error", ExecutionCount, ExceptionName, _ExceptionValue, Traceback})->
  Content = [
    {status, ?ERROR_STATUS},
    {execution_count, ExecutionCount},
    {ename, ExceptionName},
    {evalue, "ERROR"},
    {traceback, Traceback}
  ],
  ContentJson = jsx:encode(Content),
  ContentJson;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for pyout
%%      sent over the iopub socket.
generate_content_reply(pyout, {ExecutionCount, CodeOutput})->
  PyoutContent =
  try
    Data = [
    {'text/html', CodeOutput},
    {'text/plain', CodeOutput}
  ],
  DataJson = jsx:encode(Data),

  Content = [
    {execution_count, ExecutionCount},
    {data, DataJson},
    {metadata, {}}
  ],
  jsx:encode(Content)
  catch
      _:_ ->
        FrmtCode = io_lib:format("~p", [CodeOutput]),
        FrmtData = [
          {'text/html', FrmtCode},
          {'text/plain', FrmtCode}
        ],
        FrmtDataJson = jsx:encode(FrmtData),

        FrmtContent = [
        {execution_count, ExecutionCount},
        {data, FrmtDataJson},
        {metadata, {}}
        ],
        jsx:encode(FrmtContent)
  end,
  PyoutContent;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for pyin
%%      sent over the iopub socket.
generate_content_reply(pyin, {Code, ExecutionCount})->
  Content = [{execution_count, ExecutionCount},
    {code, Code}
  ],
  PyinContent = jsx:encode(Content),
  PyinContent;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for pyerr
%%      sent over the iopub socket.
generate_content_reply(pyerr, {_ExceptionName, ExecutionCount, _ExceptionValue, Traceback})->
  Content = [
    {execution_count, ExecutionCount},
    {ename, "error"},
    {evalue, "ERROR"},
    {traceback, Traceback}
  ],
  PyerrContent = jsx:encode(Content),
  PyerrContent;

%% @spec generate_content_reply(atom(), tuple()) -> list()
%% @doc Creates the content reply for display_data
%%      sent over the iopub socket.
generate_content_reply(display_data, {Source, RawData, _MetaData})->
  % Create the data dictionary with mime types as keys
  DataStruct = [
    {'text/html', RawData},
    {'text/plain', RawData}
  ],
  Data = jsx:encode(DataStruct),

  % Create the content
  Content = [
    {source, Source},
    {data, Data},
    {metadata, {}}
  ],
  DisplayContent = jsx:encode(Content),
  DisplayContent.

%% @spec create_metadata() -> list()
%% @doc Creates the metadata for the outgoing message
create_metadata()->
  Metadata = [],
  Md = jsx:encode(Metadata),
  Md.
