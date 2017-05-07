-module(ierl_msg).

-export([
         decode/2,
         encode/2
        ]).

-include("./records.hrl").

-define(DELIM, <<"<IDS|MSG>">>).


decode([UUID, Delim, HMAC, Header, ParentHeader, Metadata, Content |
        ExtraBinaries], {SignatureScheme, Key}) ->

    case Delim of
        ?DELIM -> ok
    end,

    Ctx0 = crypto:hmac_init(SignatureScheme, Key),
    Ctx1 = crypto:hmac_update(Ctx0, Header),
    Ctx2 = crypto:hmac_update(Ctx1, ParentHeader),
    Ctx3 = crypto:hmac_update(Ctx2, Metadata),
    Ctx4 = crypto:hmac_update(Ctx3, Content),

    Signature = crypto:hmac_final(Ctx4),

    case Signature of
        HMAC -> ok
    end,

    #ierl_msg{
       header = jsx:decode(Header, [return_maps]),
       parent_header = jsx:decode(ParentHeader, [return_maps]),
       metadata = jsx:decode(Metadata, [return_maps]),
       content = jsx:decode(Content, [return_maps]),

       extra_binaries = ExtraBinaries
      }.

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

    Signature = crypto:hmac_final(Ctx4),

    % TODO
    Uuid = <<"">>,

    [
     Uuid,
     ?DELIM,
     Signature,
     Header,
     ParentHeader,
     Metadata,
     Content
     | Msg#ierl_msg.extra_binaries
    ].

