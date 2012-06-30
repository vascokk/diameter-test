-module(server_cb).


-include_lib("diameter.hrl").
-include_lib("diameter_gen_base_rfc3588.hrl").

%% diameter callbacks
-export([peer_up/3,
         peer_down/3,
         pick_peer/4,
         prepare_request/3,
         prepare_retransmit/3,
         handle_answer/4,
         handle_error/4,
         handle_request/3]).


-define(UNEXPECTED, erlang:error({unexpected, ?MODULE, ?LINE})).

peer_up(_SvcName, {PeerRef, _}, State) ->
	    io:format("up: ~p~n", [PeerRef]),
	    State.

peer_down(_SvcName, {PeerRef, _}, State) ->
	    io:format("down: ~p~n", [PeerRef]),
	    State.

pick_peer(_, _, _SvcName, _State) ->
	    ?UNEXPECTED.

prepare_request(_, _SvcName, _Peer) ->
	    ?UNEXPECTED.

prepare_retransmit(_Packet, _SvcName, _Peer) ->
	    ?UNEXPECTED.

handle_answer(_Packet, _Request, _SvcName, _Peer) ->
	    ?UNEXPECTED.

handle_error(_Reason, _Request, _SvcName, _Peer) ->
	    ?UNEXPECTED.


handle_request(#diameter_packet{msg = Req, errors = []}, _SvcName, {_, Caps})
                  when is_record(Req, diameter_base_ACR) ->
            
	    #diameter_caps{origin_host = {OH,_},
		           origin_realm = {OR,_}}
	        = Caps,

            #diameter_base_ACR{'Session-Id' = Id,
		               'Accounting-Record-Type' = RecType,
                               'Accounting-Record-Number' = RecNum,
	                       'Acct-Application-Id' = AccAppId	}
	        = Req,
	  
            Ans = #diameter_base_ACA{'Result-Code' = ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_SUCCESS',
	       		       'Origin-Host' = OH,
	                       'Origin-Realm' = OR,
	                       'Session-Id' = Id,
                               'Accounting-Record-Type' = RecType,
                               'Accounting-Record-Number' = RecNum,
	                       'Acct-Application-Id' = AccAppId	

		},
		
	   %% Ans = rts_pb:encode(message),	

	    {reply, Ans}.


