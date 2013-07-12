-module(calculus).
-export([derivative/2]).

derivative(F, X) ->
  Delta = delta(),
  (F(X + Delta) - F(X)) / Delta.

delta() -> 1.0e-10.

