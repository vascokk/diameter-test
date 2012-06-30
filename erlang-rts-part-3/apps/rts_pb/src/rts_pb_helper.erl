-module(rts_pb_helper).

-export([msg_type/1, msg_code/1]).

-spec msg_type(integer()) -> atom().
msg_type(1000) -> rtserrresp;
msg_type(1001) -> rtspingreq;
msg_type(1002) -> rtspingresp;
msg_type(1003) -> rtsmsgreq;
msg_type(1004) -> rtsmsgresp;
msg_type(_)	   -> undefined.


-spec msg_code(atom()) -> integer().
msg_code(rtserrresp)  -> 1000;
msg_code(rtspingreq)  -> 1001;
msg_code(rtspingresp) -> 1002;
msg_code(rtsmsgreq)   -> 1003;
msg_code(rtsmsgresp)  -> 1004.

