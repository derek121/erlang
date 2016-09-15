-module(checker).

% A solution to http://stackoverflow.com/questions/3286020/design-pattern-function-iterating-through-a-list-in-search-of-the-first-succes

-export([check/2]).
-export([tester/1]).

-export([launch/3]).
-export([do_worker/3]).

-define(DBG, true).
-define(LOG(M, A), if ?DBG -> io:format(M ++ "~n", A); true -> ok end).

check(F, L) ->
  Self = self(),
  spawn_link(checker, launch, [Self, F, L]),
  
  ?LOG("Waiting for result in ~p", [self()]),

  receive
    success -> success;
    fail    -> fail
  end.

launch(MainParent, F, L) ->
  % spawn_link process for each element of L (of length N)
  spawn_link_workers(self(), F, L),

  ?LOG("Waiting in receive for workers in ~p", [self()]),
  
  Result = do_receive(length(L)),
  
  ?LOG("launcher unlinking from parent and exiting with result ~p and sending result to parent: ~p", 
            [Result, MainParent]),

  MainParent ! Result,
  
  unlink(MainParent),
  % Any running children will die now due to their links
  exit(kill).

spawn_link_workers(_LoopParent, _F, []) ->
  ok;
spawn_link_workers(LoopParent, F, [E | Rest]) ->
  spawn_link(checker, do_worker, [LoopParent, F, E]),
  spawn_link_workers(LoopParent, F, Rest).

do_worker(LoopParent, F, E) ->
  Self = self(),

  ?LOG("Worker ~p starting", [Self]),

  Result = F(E),

  ?LOG("Worker ~p got result ~p", [Self, Result]),
  LoopParent ! {Result, Self},
  ok.

do_receive(0) ->
  ?LOG("Done receiving - fail", []),
  fail;
do_receive(NumLeft) ->
  receive
    {success, FromPid} ->
      ?LOG("Received success from ~p", [FromPid]),
      success;
    {fail, FromPid} ->
      ?LOG("Received fail from ~p", [FromPid]),
      do_receive(NumLeft -1)
  end.

create_test_work_fun() ->
  fun(N) ->
    % Simulate work: sleep between 500 and 1500 ms
    timer:sleep(499 + rand:uniform(1001)),

    % Random result
    Result = case rand:uniform(6) of
               N -> success;
               _ -> fail
             end,

    ?LOG("test_worker for ~p: ~p", [N, Result]),
    Result
  end.

tester(N) ->
  ?LOG("tester for ~p", [N]),
  L = [rand:uniform(6) || _M <- lists:seq(1, N)],
  Result = check(create_test_work_fun(), L),

  io:format("Result: ~p~n", [Result]),
  ok.


