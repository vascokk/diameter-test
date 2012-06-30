%% @doc <p>The PB service for transmitting Diameter messages to Riak cluster.</p>
%% @end
-module(aggregation_pb_service).

-behaviour(riak_api_pb_service).

-export([init/0,
         decode/2,
         encode/1,
         process/2,
         process_stream/3]).

-spec init() -> undefined.
init() ->
    undefined.

decode(Code, <<>>) when Code==1001 ->
	{ok, rts_pb_helper:msg_type(Code)};	
decode(Code, MsgData) ->
	{ok, rts_pb:decode(rts_pg_helper:msg_type(Code), MsgData)}.


encode(Msg) when is_atom(Msg) ->
	[rts_pb_helper:msg_code(Msg)];
encode(Msg) when is_tuple(Msg) ->
	MsgType = element(1, Msg),
	[rts_pb_helper:msg_code(MsgType) | rts_pb:encode(Msg)].

process(rtspingreq, State) ->
	aggregation:ping(),
	{reply, rtspingresp, State};
process(rtsmsgreq, State) ->
	aggregation:accounting(),
	Message = rts_pb_helper:msg_type(4), %%TODO replace with the actual message
	{replay, Message, State}.

process_stream(_,_,State) ->
	{ignore, State}.


