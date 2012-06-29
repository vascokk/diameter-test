-record(diametermsg, {
    id = erlang:error({required, id}),
    name = erlang:error({required, name}),
    msg = erlang:error({required, msg})
}).

