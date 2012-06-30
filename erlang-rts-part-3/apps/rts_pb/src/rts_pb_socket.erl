-module(rts_pb_socket).
-behaviour(gen_server).

-include("rts_pb.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	         terminate/2, code_change/3]).


-type address() :: string() | atom() | inet:ip_address(). %% The TCP/IP host name or address of the Riak node
-type portnum() :: non_neg_integer(). %% The TCP port number of the Riak node's Protocol Buffers interface



-record(state, {address :: address(),            % address to connect to
		        port :: portnum(),               % port to connect to
		        sock = undefined:: port()        % gen_tcp socket 
		       }).



-spec start_link(address(), portnum()) -> {ok, pid()} | {error, term()}.
start_link(Address, Port) ->
	gen_server:start_link(?MODULE, [Address, Port], []).

-spec start(address(), portnum()) -> {ok, pid()} | {error, term()}.
start(Address, Port) ->
	gen_server:start(?MODULE, [Address, Port], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
	gen_server:call(Pid, stop).


connect(State) when State#state.sock =:= undefined ->
	#state{address = Address, port = Port} = State,
	case gen_tcp:connect(Address, Port, [binary, {active, once}, {packet, 4}, {header, 1}],infinity) of
		{ok, Sock} ->
			     {ok, State#state{sock = Sock}};
		 Error ->
			     Error
	end.


disconnect(State) ->
    case State#state.sock of
        undefined ->
            ok;
        Sock ->
            gen_tcp:close(Sock)
    end,
    NewState = State#state{sock = undefined}.
		      

send(Pid, Msg, DiameterApp, RtsApp) ->
	Req = #rtsmsgreq{msg = erlang:term_to_bynary(Msg), diameter_application = DiameterApp, rts_application = RtsApp},
	gen_server:call(Pid, {req, Req}, infinity).




init([Address, Port]) ->
	State = #state{address = Address, port = Port},
	case connect(State) of
	        {ok, NewState} ->
		                      ok;
	        {error, Reason} ->
							disconnect(State)	
	end.

handle_call({req, Req}, From, State) when State#state.sock =/= undefined ->
	Pkt = rts_pb_codec:encode(Req),
	ok = gen_tcp:send(State#state.sock, Pkt).


handle_info({tcp_error, _Socket, Reason}, State) ->
	error_logger:error_msg("PBC client TCP error for ~p:~p - ~p\n",
	[State#state.address, State#state.port, Reason]),
	disconnect(State);

handle_info({tcp, Sock, Data}, State=#state{sock = Sock}) ->
	[MsgCode|MsgData] = Data,
	Resp = rts_pb:decode(MsgCode, MsgData),
	case Resp of
		#rtserrresp{} ->
			%%NewState1 = maybe_reply(on_error(Active, Resp, State)),
			%%NewState = dequeue_request(NewState1#state{active = undefined});
			error_logger:error_msg("Error receieving PB message ~p:~p - ~p\n",
									[State#state.address, State#state.port, rtserrorresp]);
		_ ->
			case process_response(Resp, State) of
				{reply, Response, State} ->
					{reply, Response, State};
				_ ->
					{replay, not_supported, State}
			end
	end;
	%%ok = inet:setopts(Sock, [{active, once}]),
	%%{noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
	disconnect(State).




handle_cast(_Msg, State) ->
		    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

process_response(rpbpingresp, State) ->
	{reply, pong, State};
process_response(#rtsmsgreq{msg = DiameterMsgAnswer}, State) ->
	{reply, DiameterMsgAnswer, State}.



