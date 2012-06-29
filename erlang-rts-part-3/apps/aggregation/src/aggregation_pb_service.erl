%% @doc <p>The PB service for transmitting Diameter messages to Riak cluster.</p>
%%
%% <pre>
%%   1001 - PbDiaMsgReq
%% </pre>
%%
%% <p>This service produces the following responses:</p>
%%
%% <pre>
%%   1002 - PbDiaMsgResp
%% </pre>
%%
%% @end
-module(aggregation_pb_service).

-include_lib("riak_pb/include/riak_pb.hrl").

-behaviour(riak_api_pb_service).

-export([init/0,
         decode/2,
         encode/1,
         process/2,
         process_stream/3]).

%% @doc init/0 callback. Returns the service internal start
%% state. This service has no state.
-spec init() -> undefined.
init() ->
    undefined.

%% @doc decode/2 callback. Decodes an incoming message.
%% @todo Factor this out of riakc_pb to remove the dependency.
decode(Code, Bin) ->
    %% {ok, riak_pb_codec:decode(Code, Bin)}.
    {ok, aggregaton_pb:decode(Code, Bin)}.

%% @doc encode/1 callback. Encodes an outgoing response message.
%% @todo Factor this out of riakc_pb to remove the dependency.
encode(Message) ->
    %% {ok, riak_pb_codec:encode(Message)}.
    {ok, encode_msg(Message)}.    

%% @doc Create an iolist of msg code and protocol buffer
%% message. Replaces `riakc_pb:encode/1'.
-spec encode_msg(atom() | tuple()) -> iolist().
encode_msg(Msg) when is_atom(Msg) ->
    [msg_code(Msg)];
encode_msg(Msg) when is_tuple(Msg) ->
    MsgType = element(1, Msg),
    Encoder = encoder_for(MsgType),
    [msg_code(MsgType) | aggregation_pb:encode(Msg)].


process(diametermsgreq, State) ->

	{reply, message, State}.
%%%% @doc process/2 callback. Handles an incoming request message.
%%process(rpbpingreq, State) ->
%%    {reply, rpbpingresp, State};
%%process(rpbgetserverinforeq, State) ->
%%    %% TODO: Think of a better way to present the server version
%%    {ok, Vsn} = application:get_key(riak_kv, vsn),
%%    Message = #rpbgetserverinforesp{node = riak_pb_codec:to_binary(node()),
%%                                    server_version = riak_pb_codec:to_binary(Vsn)},
%%    {reply, Message, State}.

process_stream(_,_,State) ->
	{ignore, State}.

%%%% @doc process_stream/3 callback. Handles a streaming message
%%%% received by the server on behalf of the service. This service
%%%% implements no streaming responses, so all messages are ignored.
%%process_stream(_,_,State) ->
%%    {ignore, State}.

%% @doc Converts a message code into the symbolic message
%% name. Replaces `riakc_pb:msg_type/1'.
-spec msg_type(integer()) -> atom().
msg_type(0) -> rpberrorresp;
msg_type(1) -> rpbpingreq;
msg_type(2) -> rpbpingresp;
msg_type(3) -> rpbgetclientidreq;
msg_type(4) -> rpbgetclientidresp;
sg_type(_) -> undefined.

%% @doc Converts a symbolic message name into a message code. Replaces
%% `riakc_pb:msg_code/1'.
-spec msg_code(atom()) -> integer().
msg_code(rpberrorresp)           -> 0;
msg_code(rpbpingreq)             -> 1;
msg_code(rpbpingresp)            -> 2;
msg_code(rpbgetclientidreq)      -> 3;
msg_code(rpbgetclientidresp)     -> 4.


