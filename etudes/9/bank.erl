-module(bank).
-export([account/1]).

% logger for all
% non-numbers
% catch errors
% have to hit return after error to get prompt
% displaying in e
% doesn't display in floating at first

account(Balance) ->
  Operation = get_operation(),
  Result = handle_operation(Operation, Balance),
  case Result of
    quit       -> ok; % A no-op?
    ok         -> account(Balance);
    NewBalance -> account(NewBalance)
  end.


get_operation() ->
  Input = string:to_lower(read_char("D)eposit, W)ithdraw, B)alance, Q)uit: ")),
  input_to_operation(Input).


read_char(Prompt) ->
  string:substr(io:get_line(Prompt), 1, 1).


input_to_operation(Input) ->
  case Input of
    "w"     -> withdraw;
    "d"     -> deposit;
    "b"     -> balance;
    "q"     -> quit;
    "\n"    -> ok;
    Unknown -> io:format("Unknown command: ~p~n", [Unknown]),
               ok
  end.


handle_operation(quit, _) -> quit;

handle_operation(ok, _) -> ok;

handle_operation(balance, Balance) -> 
  io:format("Your balance is ~p~n", [Balance]),
  Balance;

handle_operation(Operation, Balance) ->
  Amount = get_amount(Operation),
  NewBalance = process(Operation, Amount, Balance),
  output_balance(Operation, NewBalance),
  NewBalance.
  

get_amount(Operation) ->
  OperationPrompt = operation_to_prompt(Operation),
  Prompt = "Amount to " ++ OperationPrompt ++ ": ",
  read_line(Prompt).
  

read_line(Prompt) ->
  Line = io:get_line(Prompt),
  String = string:substr(Line, 1, length(Line) - 1),
  to_float(String).


to_float(String) ->
  {Float, _} = string:to_float(String),
  case Float of
    error -> {Int, _} = string:to_integer(String),
             Int;
    _     -> Float
  end.


operation_to_prompt(withdraw) -> "withdraw";
operation_to_prompt(deposit)  -> "deposit".
%operation_to_prompt(balance)  -> "balance";
%operation_to_prompt(quit)     -> "quit".


process(withdraw, Amount, _) when Amount < 0 ->
  error_logger:error_report("Amount may not be less than 0");
process(withdraw, Amount, Balance) when Amount =< Balance -> 
  Balance - Amount;
process(withdraw, Amount, Balance) ->
  error_logger:error_msg("Amount to withdraw (~p) exceeds balance (~p)~n", [Amount, Balance]);

process(deposit, Amount, _) when Amount < 0 ->
  error_logger:error_report("Amount may not be less than 0");
process(deposit, Amount, Balance) when Amount >= 10000 ->
  error_logger:warning_msg("Your deposit of ~p may be subject to hold~n", [Amount]),
  deposit(Amount, Balance); % Duplicate
process(deposit, Amount, Balance) ->
  deposit(Amount, Balance). % Duplicate



deposit(Amount, Balance) -> Balance + Amount.


output_balance(_, ok) -> ok;
output_balance(withdraw, Balance) ->
  io:format("New balance: ~p~n", [Balance]);
output_balance(deposit, Balance) ->
  io:format("New balance: ~p~n", [Balance]);
output_balance(balance, Balance) ->
  io:format("Balance: ~p~n", [Balance]).
  
