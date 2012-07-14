-module(aggregation_pb_service_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").



pb_test_() ->
    [{"ping encode decode",
      ?_test(begin
		%%rts_aggr_ping_req =:= aggregation_pb_service:decode(aggregation_pb_service:encode(rts_aggr_ping_req)), 	
		Encoded = aggregation_pb_service:encode(rtspingreq),
		Decoded = aggregation_pb_service:decode(hd(Encoded), <<>>),
		?assertEqual({ok, rtspingreq}, Decoded)
             end)}
	].
   



