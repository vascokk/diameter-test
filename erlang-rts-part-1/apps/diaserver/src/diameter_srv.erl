%%%'   HEADER
%%% @author Vasco Kolarov <vas@vas.io>
%%% @copyright 2012 Vasco Kolarov
%%% @doc gen_server callback module implementation:
%%% 
%%% @end
-module(diameter_srv).
-author('Vasco Kolarov <vas@vas.io>').

-behaviour(gen_server).

-include_lib("diameter.hrl").
-include_lib("diameter_gen_base_rfc3588.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).
-export([stop/0, terminate/2]).

% TODO: If unnamed server, remove definition below.
-define(SERVER, ?MODULE).
%%%.
%%%'   PUBLIC API

%% @doc starts gen_server implementation and caller links to the process too.
-spec start_link() -> {ok, Pid} | ignore | {error, Error}
  when
      Pid :: pid(),
      Error :: {already_started, Pid} | term().
start_link() ->
  % TODO: decide whether to name gen_server callback implementation or not.
  % gen_server:start_link(?MODULE, [], []). % for unnamed gen_server
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc stops gen_server implementation process
-spec stop() -> ok.
stop() ->
  gen_server:cast(?SERVER, stop).

% TODO: add more public API here...

%%%.
%%%'   CALLBACKS
%% @callback gen_server
init(State) ->
        SvcName = ?MODULE,
        SvcOpts = [{'Origin-Host', atom_to_list(SvcName) ++ ".example.com"},
                        {'Origin-Realm', "example.com"},
                        {'Vendor-Id', 193},
                        {'Product-Name', "Server"},
                        {'Auth-Application-Id', [?DIAMETER_APP_ID_COMMON]},
                        {application, [{alias, diameter_base_app},
                                       {dictionary, ?DIAMETER_DICT_COMMON},
                                       {module, server_cb}]}],
        TransportOpts =  [{transport_module, diameter_tcp},
                                        {transport_config, [{reuseaddr, true},
                                        {ip, {127,0,0,1}}, {port, 3868}]}],
        diameter:start_service(SvcName, SvcOpts),
        diameter:add_transport(SvcName, {listen, TransportOpts}),
       {ok, State}.

%% @callback gen_server
handle_call(_Req, _From, State) ->
  {reply, State}.

%% @callback gen_server
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Req, State) ->
  {noreply, State}.

%% @callback gen_server
handle_info(_Info, State) -> 
  {noreply, State}.

%% @callback gen_server
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @callback gen_server
terminate(normal, _State) ->
  ok;
terminate(shutdown, _State) ->
  ok;
terminate({shutdown, _Reason}, _State) ->
  ok;
terminate(_Reason, _State) ->
  ok.

%%%.
%%%'   PRIVATE FUNCTIONS
% TODO: Add private helper functions here.

%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:
