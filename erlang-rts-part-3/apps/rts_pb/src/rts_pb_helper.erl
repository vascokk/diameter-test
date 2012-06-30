-module(rts_pb_helper).

-export([msg_type/1, msg_code/1]).

-spec msg_type(integer()) -> atom().
msg_type(0) -> rts_aggr_err_resp;
msg_type(1) -> rts_aggr_ping_req;
msg_type(2) -> rts_aggr_ping_resp;
msg_type(3) -> rts_aggr_msg_req;
msg_type(4) -> rts_aggr_msg_resp.


-spec msg_code(atom()) -> integer().
msg_code(rts_aggr_err_resp)             -> 0;
msg_code(rts_aggr_ping_req)             -> 1;
msg_code(rts_aggr_ping_resp)            -> 2;
msg_code(rts_aggr_msg_req)              -> 3;
msg_code(rts_aggr_msg_resp)             -> 4.

