-module(ranking).
-export([top100/1]).
-spec top100([data()]) -> [data()].
-type data() :: {atom(), number()}.

top100(DataList) ->
    SortedList = sort(DataList),
    if 
        length(DataList) =< 100 ->
            SortedList;
        true ->
            lists:sublist(SortedList, 100)
    end.

sort([{_, H} | L]) ->
    BigL = [{N, R} || {N, R} <- L, H >= R],
    LitL = [{N, R} || {N, R} <- L, H < R],
    sort(BigL) ++ sort(LitL);

sort([]) -> [].




