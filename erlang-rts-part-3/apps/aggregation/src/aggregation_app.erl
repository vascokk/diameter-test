-module(aggregation_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case aggregation_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, aggregation_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(aggregation_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(aggregation_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(aggregation, self()),
			ok = riak_api_pb_service:register(aggregation_pb_service, 1000, 1004),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
