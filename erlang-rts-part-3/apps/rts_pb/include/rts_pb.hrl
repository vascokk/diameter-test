-record(rtserrresp, {
    errmsg = erlang:error({required, errmsg}),
    errcode = erlang:error({required, errcode}),
    rts_application
}).

-record(rtsmsgreq, {
    msg = erlang:error({required, msg}),
    diameter_application,
    rts_application
}).

-record(rtsmsgresp, {
    msg = erlang:error({required, msg}),
    diameter_application,
    rts_application
}).

