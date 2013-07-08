-module(stats).
-export([minimum/1, maximum/1, range/1]).

%%
minimum([X|Xs]) ->
  minimum(Xs, X).

minimum([], Min) ->
  Min;

minimum([X|Xs], Min) when X < Min ->
  minimum(Xs, X);

minimum([_|Xs], Min) ->
  minimum(Xs, Min).

%%
maximum([X|Xs]) ->
  maximum(Xs, X).

maximum([], Max) ->
  Max;

maximum([X|Xs], Max) when X > Max ->
  maximum(Xs, X);

maximum([_|Xs], Max) ->
  maximum(Xs, Max).

%%
range([X|Xs]) ->
  Min = minimum([X|Xs]),
  Max = maximum([X|Xs]),
  [Min, Max].

  



