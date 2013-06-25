-module(e4_1).
-export([area/3]).

area(Type, H, W) when H > 0, W > 0 ->
  case Type of
    rectangle -> H * W;
    triangle  -> H * W / 2.0;
    ellipse   -> math:pi()  * H * W;
    _         -> 0
  end;

area(_, _, _) -> 0.

