-module(dates).
-export([date_parts/1]).

%%-spec(date_parts(String()) -> list()).
date_parts(Date) ->
  %% 2013-01-21
  [Yy, Mm, Dd] = re:split(Date, "-", [{return, list}]),


