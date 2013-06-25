-module(nth_root).
-export([nth_root/2]).

%%
nth_root(X, N) -> nth_root(X, N, X / 2.0).

nth_root(X, N, A) ->
  io:format("Current guess is ~p~n", [A]),
  Limit  = limit(),
  F      = raise(A, N) - X,
  Fprime = N * raise(A, N - 1),
  Next   = A - F / Fprime,
  Change = abs(Next - A),
  if Change < Limit  -> Next;
     Change >= Limit -> nth_root(X, N, Next)
  end.
   
%%
limit() -> raise(10, -8).


