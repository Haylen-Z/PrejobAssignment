-module(resourceManager_ser).
-behaviour(gen_server).
-export([start_link/2, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {x, z, allocMap}).

start() ->
    start_link(10, 5000).

start_link(X, Z) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [X, Z], []).

init([X, Z]) ->
    {ok, #state{x = X, z = Z, allocMap = #{}}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(Request, From, State) ->
    case Request of
        {alloc, Y} ->
           alloc(From, State, Y);
        {free, Y} ->
            free(From, State, Y);
        {timeout, F, Y} ->
            free(F, State, Y);
        _ ->
            {reply, error, State}
    end.

alloc(_, #state{x=X}=State, Y) when Y > X; Y =< 0 ->
    {reply, {fail, noResources}, State};

alloc({Pid, _} = From, State, Y) ->
    #state{x = X, z = Z, allocMap = AllocMap} = State,
    NewMap = maps:put(Pid, maps:get(Pid, AllocMap, 0) + Y, AllocMap),
    timer:apply_after(Z, gen_server, call, [?MODULE, {timeout, From, Y}]),
    {reply, ok, State#state{x = X - Y, allocMap = NewMap}}.


free({Pid, _}, State, Y) ->
    #state{x = X, allocMap = Map} = State,
    Res = maps:get(Pid, Map, 0),
    if 
        Res =< Y ->
            NewMap = maps:remove(Pid, Map),
            Nx = Res;
        true ->
            Nx = Y,
            NewMap = maps:put(Pid, Res - Y, Map)
    end,
    {reply, ok, State#state{x = X + Nx, allocMap = NewMap}}.

handle_cast(_Msg, State) ->
    {noreply, State}.     

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
