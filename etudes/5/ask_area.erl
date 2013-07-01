-module(ask_area).
-export([area/0]).

%%-spec(area() -> number()).
area() ->
  Choice = get_choice(),
  %%io:format("Choice: ~s~n", [Choice]).
  Shape = char_to_shape(Choice),
  %%io:format("Shape: ~p~n", [Shape]).
  {N1, N2} = get_dimensions(Shape),
  %%io:format("Dim: ~p ~p~n", [N1, N2]).
  Area = calculate(Shape, N1, N2),
  Area.

get_choice() -> 
  string:substr(io:get_line("R)ectangle, T)riangle, or E)llipse > "), 1, 1).

%%-spec(char_to_shape(String()) -> atom()).
char_to_shape(Char) ->
  case string:to_lower(Char) of
    "r" -> rectangle;
    "t" -> triangle;
    "e" -> ellipse;
    _   -> unknown
  end.

-spec(get_dimensions(atom()) -> {number(), number()}).
get_dimensions(Shape) ->
  {Prompt1, Prompt2} = case Shape of
    rectangle -> {"Enter width",      "Enter height"};
    triangle  -> {"Enter base",       "Enter height"};
    ellipse   -> {"Enter major axis", "Enter minor axis"};
    _         -> {"Enter val 1",      "Enter val 2"}
  end,
  get_dimensions(Prompt1, Prompt2).
  

%%-spec(get_dimensions(String(), String()) -> {number(), number()}).
get_dimensions(Prompt1, Prompt2) ->
  N1 = get_number(Prompt1),
  N2 = get_number(Prompt2),
  {N1, N2}.

%%-spec(get_number(String()) -> number()).
get_number(Prompt) ->
  S = io:get_line(Prompt ++ " > "),

  {Float, _} = string:to_float(S),
  %%io:format("Tmp: ~p~n", [X]),

  case Float of
    error -> {Int, _} = string:to_integer(S),
             Int;
    _     -> Float
  end.

%%-spec(calculate(atom(), number(), number()) -> number()).
calculate(unknown, _, _) ->
  io:format("Unknown shape~n");
  
calculate(_, N1, N2) when N1 < 0; N2 < 0 ->
  io:format("Negative value~n");

calculate(_, N1, N2) when not (is_number(N1) and is_number(N2)) ->
  io:format("Non-numerical value~n");

calculate(Shape, N1, N2) ->
  e4_1:area(Shape, N1, N2).

