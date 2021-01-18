%%%-------------------------------------------------------------------
%% @doc resourceManager public API
%% @end
%%%-------------------------------------------------------------------

-module(resourceManager_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    resourceManager_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
