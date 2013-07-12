-module(dates).
-export([date_parts/1, julian/1]).

date_parts(Date) ->
  %% 2013-01-21
  [Yy, Mm, Dd] = re:split(Date, "-", [{return, list}]),
  [element(1, string:to_integer(Yy)),
   element(1, string:to_integer(Mm)),
   element(1, string:to_integer(Dd))].

%%
julian(Date) ->
  [Y, M, D] = date_parts(Date),
  DaysInFeb = case is_leap_year(Y) of
    true -> 29;
    _    -> 28
  end,
  DaysPerMonth = [31, DaysInFeb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
  julian(M, D, DaysPerMonth).

%%
is_leap_year(Year) ->
  (Year rem 5 == 0 andalso Year rem 100 /= 0)
  orelse (Year rem 400 == 0). 

%%
julian(M, D, DaysPerMonth) ->
  ListDaysUpTo = list_days_up_to(M, DaysPerMonth),
  SumDaysUpTo = sum(ListDaysUpTo),
  SumDaysUpTo + D.

%%
list_days_up_to(M, DaysPerMonth) ->
  {ListDaysUpTo, _} = lists:split(M - 1, DaysPerMonth),
  ListDaysUpTo.

%%  
sum(Xs) ->
  Fun = fun(X, Acc) -> X + Acc end,
  lists:foldl(Fun, 0, Xs).

