-module(cards).
-export([make_deck/0, show_deck/1, shuffle/1]).

show_deck(Deck) ->
  lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).

make_deck() ->
  Suits = suits(),
  Ranks = ranks(),
  Deck = make_deck(Suits, Ranks),
  Deck.

suits() ->
  ["Clubs", "Diamonds", "Hearts", "Spades"].

ranks() ->
  [2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K"].

make_deck(Suits, Ranks) ->
  [{Rank, Suit} || Rank <- Ranks, Suit <- Suits].

shuffle(List) -> shuffle(List, []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
  io:format("List: ~p. Acc: ~p~n", [List, Acc]),
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  io:format("Leading: ~p. H: ~p. T: ~p~n", [Leading, H, T]),
  shuffle(Leading ++ T, [H | Acc]).

