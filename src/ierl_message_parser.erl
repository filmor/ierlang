%%%-------------------------------------------------------------------
%%% @author Robbie Lynch <robbie.lynch@outlook.com>
%%% @copyright (C) 2014, Robbie Lynch
%%% @doc Module to parse IPython messages and their contents
%%%
%%% @end
%%% Created : 03. Apr 2014 10:15
%%%-------------------------------------------------------------------
-module(ierl_message_parser).
-author("Robbie Lynch").
-export([parse_content/2, parse_header/1]).

%% @spec parse_content(list(), atom()) -> tuple()
%% @doc Parses and extracts the Content items from the IPython execute_request message
parse_content(Content, execute_request)->
    MsgPropList = jsx:decode(Content),
    Code = binary_to_list(proplists:get_value(<<"code">>, MsgPropList)),
    Silent = proplists:get_value(<<"silent">>, MsgPropList),
    StoreHistory = proplists:get_value(<<"store_history">>, MsgPropList),
    UserVariables = proplists:get_value(<<"user_variables">>, MsgPropList),
    UserExpressions = proplists:get_value(<<"user_expressions">>, MsgPropList),
    AllowStdin = proplists:get_value(<<"allow_stdin">>, MsgPropList),
    {ok, Code, Silent, StoreHistory, UserVariables, UserExpressions, AllowStdin};

%% @spec parse_content(list(), atom()) -> tuple()
%% @doc Parses and extracts the Content items from the IPython complete_request message
parse_content(Content, complete_request)->
    MsgPropList = jsx:decode(Content),
    Text = binary_to_list(proplists:get_value(<<"text">>, MsgPropList)),
    Line = proplists:get_value(<<"line">>, MsgPropList),
    Block = proplists:get_value(<<"block">>, MsgPropList),
    CursorPos = proplists:get_value(<<"cursor_pos">>, MsgPropList),
    {ok, Text, Line, Block, CursorPos}.

%% @spec parse_header(list()) -> tuple()
%% @doc Parses and extracts the header items from the IPython header message.
parse_header(Header)->
    print("Got header:", [Header]),
    HeaderPropList = jsx:decode(Header),
    %Date = extract_date_from_header(HeaderPropList),
    Username = proplists:get_value(<<"username">>, HeaderPropList),
    Session = proplists:get_value(<<"session">>, HeaderPropList),
    MessageID = proplists:get_value(<<"msg_id">>, HeaderPropList),
    MessageType = proplists:get_value(<<"msg_type">>, HeaderPropList),
    print("[MessageParser] Message Type Received = ", [MessageType]),
    {ok, Username, Session, MessageID, MessageType, ""}.


%% @doc Function to print stuff if debugging is set to true
print(Prompt, Stuff)->
    io:format(string:concat(Prompt, "~p~n"), [Stuff]).
