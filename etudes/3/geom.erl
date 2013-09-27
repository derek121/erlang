-module(geom).
-export([area/1, area/3]).

area(rectangle, A, B) when A >= 0, B >= 0 -> A * B;
area(triangle, A, B)  when A >= 0, B >= 0 -> A * B / 2;
area(ellipse, A, B)   when A >= 0, B >= 0 -> math:pi() * A * B;
area(_, _, _)                             -> 0.

area({Shape, A, B})                       -> area(Shape, A, B).

