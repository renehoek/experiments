%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2023 15:22
%%%-------------------------------------------------------------------
-module(drop).
-author("renevanhoek").

%% API
-export([drop/0, fall_velocity/2]).

%% @doc Calculates the velocity of an object falling on Earth
%% as if it were in a vacuum (no air resistance).  The distance is
%% the height from which the object falls, specified in meters,
%% and the function returns a velocity in meters per second.

drop() ->
  receive
    {From, Planemo, Distance} ->
      From ! {Planemo, Distance, fall_velocity(Planemo, Distance)},
      drop()
  end.

fall_velocity(Planemo, Distance) when Distance >= 0  ->
  Gravity = case Planemo of
    earth -> 9.8;
    moon ->  1.6;
    mars ->  3.71  % no closing period!
  end,

  try math:sqrt(2 * Gravity * Distance) of
    Velocity -> Velocity
  catch
    error:Error -> {error, Error}
  end.

