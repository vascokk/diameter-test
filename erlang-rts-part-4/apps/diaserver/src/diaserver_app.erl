%%%'   HEADER
%%% @author Vasco Kolarov <vas@vas.io> 
%%% @since 
%%% @copyright 2012 Vasco Kolarov
%%% @doc 
%%% @end

-module(diaserver_app).
-behaviour(application).

-export([start/2, stop/1]).
%%%.
%%%'   CALLBACKS
start(_StartType, _StartArgs) ->
  diaserver_sup:start_link().

stop(_State) ->
  ok.
%%%.
%%%'   PRIVATE FUNCTIONS

%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:
