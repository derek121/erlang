-module(stats).
-export([minimum/1, maximum/1, range/1, mean/1, stdv/1]).

%%
mean(Xs) ->
  try
    Sum = sum(Xs),
    Len = length(Xs),
    Sum / Len
  catch
    error:Error -> {error, Error}
  end.

%%
sum(Xs) ->
  Fun = fun(X, Acc) -> X + Acc end,
  lists:foldl(Fun, 0, Xs).

%%
stdv(Xs) ->
  try
    {Sum, SumOfSquares} = sum_and_sum_of_squares(Xs),
    N = length(Xs),
    math:sqrt(((N * SumOfSquares) - (Sum * Sum)) / (N * (N - 1)))
  catch
    error:Error -> {error, Error}
  end.
  
%%
sum_and_sum_of_squares(Xs) ->
  Fun = fun(X, Acc) -> {Sum, SumOfSquares} = Acc, {X + Sum, X * X + SumOfSquares} end,
  lists:foldl(Fun, {0, 0}, Xs).

%%
minimum([X|Xs]) ->
  try
    minimum(Xs, X)
  catch
    error:Error -> {error, Error}
  end.

minimum([], Min) ->
  Min;

minimum([X|Xs], Min) when X < Min ->
  minimum(Xs, X);

minimum([_|Xs], Min) ->
  minimum(Xs, Min).

%%
maximum([X|Xs]) ->
  try
    maximum(Xs, X)
  catch
    error:Error -> {error, Error}
  end.

maximum([], Max) ->
  Max;

maximum([X|Xs], Max) when X > Max ->
  maximum(Xs, X);

maximum([_|Xs], Max) ->
  maximum(Xs, Max).

%%
range([X|Xs]) ->
  try
    Min = minimum([X|Xs]),
    Max = maximum([X|Xs]),
    [Min, Max]
  catch
    error:Error -> {error, Error}
  end.

