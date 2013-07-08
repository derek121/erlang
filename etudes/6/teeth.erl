-module(teeth).
-export([alert/1]).

alert([List|Lists]) ->
  alert(List, Lists, 1, []).

%%
alert([], _, _, Acc) ->
  lists:reverse(Acc);

%% Don't like the duplicated code in this and the next function
alert(List, [], N, Acc) ->
  NewAcc = check_list(List, N, Acc),
  alert([], [], N + 1, NewAcc);
  
alert(List, Lists, N, Acc) ->
  NewAcc = check_list(List, N, Acc),
  [NextList | NextLists] = Lists,
  alert(NextList, NextLists, N + 1, NewAcc).

%%
check_list(List, N, Acc) ->
  Fun = fun(X) -> if X >= 4 -> true; true -> false end end,
  NewAcc = case lists:any(Fun, List) of
    true -> [N | Acc];
    _    -> Acc
  end,
  NewAcc.

