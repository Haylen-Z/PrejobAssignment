-module(skillJudgment).
-export([judge/8]).

judge(X1, X2, Y1, Y2, X3, X4, Y3, Y4) when X1 > X4; X3 > X2; Y1 > Y4; Y3 > Y2 -> 
    false;

judge(_, _, _, _, _, _, _, _) ->
    true.

    


