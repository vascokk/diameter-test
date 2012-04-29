-module(server).

-include_lib("diameter.hrl").
-include_lib("diameter_gen_base_rfc3588.hrl").

-export([start/0,
	listen/0]).

-define(SVC_NAME,     ?MODULE).
-define(APP_ALIAS,    ?MODULE).
-define(CALLBACK_MOD, server_cb).

-define(DEFAULT_ADDR, {127,0,0,1}).
-define(DEFAULT_PORT, 3868).

-define(SVC_OPTS(Name), [{'Origin-Host', atom_to_list(Name) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
			{application, [{alias, ?APP_ALIAS},
                                       {dictionary, ?DIAMETER_DICT_COMMON},
                                       {module, ?CALLBACK_MOD}]},
                        {'Acct-Application-Id', [?DIAMETER_APP_ID_ACCOUNTING]},
                        {application, [{alias, ?APP_ALIAS},
                                       {dictionary, ?DIAMETER_DICT_ACCOUNTING},
                                       {module, ?CALLBACK_MOD}]}]).


-define(TRANSPORT_OPTS(Addr, Port), [{transport_module, diameter_tcp},
     					{transport_config, [{reuseaddr, true},
                        {ip, Addr},
                        {port, Port}]}]).

start() ->
	diameter:start(),
	diameter:start_service(?SVC_NAME, ?SVC_OPTS(?SVC_NAME)).

listen() ->
	diameter:add_transport(?SVC_NAME, {listen, ?TRANSPORT_OPTS(?DEFAULT_ADDR, ?DEFAULT_PORT)}).
