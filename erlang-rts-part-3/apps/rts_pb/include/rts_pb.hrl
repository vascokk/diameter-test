-record(diametererrresp, {
    errmsg = erlang:error({required, errmsg}),
    errcode = erlang:error({required, errcode})
}).

-record(diametermsgreq, {
    errmsg = erlang:error({required, errmsg}),
    errcode = erlang:error({required, errcode})
}).

-record(diametermsgresp, {
    errmsg = erlang:error({required, errmsg}),
    errcode = erlang:error({required, errcode})
}).

