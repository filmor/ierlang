-module(ierl_msg).

-export([
         decode/2,
         encode/2,
         msg_type/1,
         msg_id/1,
         add_headers/3
        ]).

-include("./records.hrl").

-define(DELIM, <<"<IDS|MSG>">>).
-define(VERSION, <<"5.1">>).


-spec decode([binary()], {crypto:hash_algorithms(), binary()}) -> #ierl_msg{}.
decode(MultipartMsg, {SignatureScheme, Key}) ->
    {Uuids, Suffix} = ierl_util:split_at_delim(MultipartMsg, ?DELIM),

    [HMAC, Header, ParentHeader, Metadata, Content | ExtraBinaries] = Suffix,

    Ctx0 = crypto:hmac_init(SignatureScheme, Key),
    Ctx1 = crypto:hmac_update(Ctx0, Header),
    Ctx2 = crypto:hmac_update(Ctx1, ParentHeader),
    Ctx3 = crypto:hmac_update(Ctx2, Metadata),
    Ctx4 = crypto:hmac_update(Ctx3, Content),

    Signature = ierl_util:hexlify(crypto:hmac_final(Ctx4)),

    case Signature of
        HMAC -> ok;
        _ -> error(invalid_signature)
    end,

    #ierl_msg{
       uuids = Uuids,
       header = jsx:decode(Header, [return_maps]),
       parent_header = jsx:decode(ParentHeader, [return_maps]),
       metadata = jsx:decode(Metadata, [return_maps]),
       content = jsx:decode(Content, [return_maps]),

       extra_binaries = ExtraBinaries
      }.


-spec encode(#ierl_msg{}, {crypto:hash_algorithms(), binary()}) -> [binary()].
encode(#ierl_msg{} = Msg, {SignatureScheme, Key}) ->
    Ctx0 = crypto:hmac_init(SignatureScheme, Key),
    Header = jsx:encode(Msg#ierl_msg.header),
    Ctx1 = crypto:hmac_update(Ctx0, Header),
    ParentHeader = jsx:encode(Msg#ierl_msg.parent_header),
    Ctx2 = crypto:hmac_update(Ctx1, ParentHeader),
    Metadata = jsx:encode(Msg#ierl_msg.metadata),
    Ctx3 = crypto:hmac_update(Ctx2, Metadata),
    Content = jsx:encode(Msg#ierl_msg.content),
    Ctx4 = crypto:hmac_update(Ctx3, Content),

    Signature = ierl_util:hexlify(crypto:hmac_final(Ctx4)),

    % TODO
    Msg#ierl_msg.uuids ++ [
     ?DELIM,
     Signature,
     Header,
     ParentHeader,
     Metadata,
     Content
     | Msg#ierl_msg.extra_binaries
    ].


header_entry(#ierl_msg{header=Header}, Key) ->
    BinKey = ierl_util:ensure_binary(Key),
    maps:get(BinKey, Header).


-spec msg_type(#ierl_msg{}) -> binary().
msg_type(#ierl_msg{} = Msg) ->
    header_entry(Msg, msg_type).

-spec msg_id(#ierl_msg{}) -> binary().
msg_id(#ierl_msg{} = Msg) ->
    header_entry(Msg, msg_id).


-spec add_headers(#ierl_msg{}, #ierl_msg{}, atom() | binary()) -> #ierl_msg{}.
add_headers(Msg = #ierl_msg{}, Parent = #ierl_msg{}, MessageType) ->
    Header = #{
      <<"date">> => iso8601:format(os:timestamp()),
      <<"username">> => header_entry(Parent, username),
      <<"session">> => header_entry(Parent, session),
      <<"msg_type">> => ierl_util:ensure_binary(MessageType),
      <<"msg_id">> => msg_id(Parent),
      <<"version">> => ?VERSION
     },

    Msg#ierl_msg{
      uuids=Parent#ierl_msg.uuids,
      header=Header,
      parent_header=Parent#ierl_msg.header
     }.
