-module(people).
-export([male_and_over_40/0, male_or_over_40/0]).

male_and_over_40() ->
  People = people(),
  [Name || {Name, Gender, Age} <- People, Age > 40, Gender == $M]. 

male_or_over_40() ->
  People = people(),
  [Name || {Name, Gender, Age} <- People, (Age > 40) orelse (Gender == $M)]. 

people() ->
  [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
   {"Tran", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}].

