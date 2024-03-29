%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2023 17:23
%%%-------------------------------------------------------------------
-module(mph_drop).
-author("renevanhoek").

%% API
-export([mph_drop/0]).

mph_drop() ->
  process_flag(trap_exit, true),
  Drop=spawn_link(drop,drop,[]),
  convert(Drop).

convert(Drop) ->
  receive
    {Planemo, Distance} ->
      Drop ! {self(), Planemo, Distance},
      convert(Drop);
    {'EXIT', Pid, Reason} ->
      io:format("Failure: ~p died, reason: ~p. Respawning.~n", [Pid, Reason]),
      NewDrop=spawn_link(drop,drop,[]),
      convert(NewDrop);
    {Planemo, Distance, Velocity} ->
      MphVelocity= 2.23693629 * Velocity,
      io:format("On ~p, a fall of ~p meters yields a velocity of ~p mph.~n", [Planemo, Distance, MphVelocity]),
      convert(Drop)
  end.