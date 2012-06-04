%%%'   HEADER
%%% @author Vasco Kolarov <vas@vas.io> 
%%% @since 
%%% @copyright 2012 Vasco Kolarov
%%% @doc 
%%% @end
-module(diaserver_sup).

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
%%%.
%%%'   PUBLIC API
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%.
%%%'   CALLBACKS
init([]) ->
        DiaServer = {diaserver,{diameter_srv,start_link,[]},
                     permanent,
                     5000,
                     worker,
                     [server_cb]},

        {ok, { {one_for_one, 5, 10}, [DiaServer]} }.

%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:
