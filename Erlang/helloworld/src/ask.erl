%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2023 16:36
%%%-------------------------------------------------------------------
-module(ask).
-author("renevanhoek").

%% API
-export([line/0]).

line() ->
  Planemo = get_planemo(),
  Distance = get_distance(),
  drop:fall_velocity(Planemo, Distance).


get_planemo() ->
  io:format("Where are you?~n"),
  io:format(" 1. Earth ~n"),
  io:format(" 2. Earth's Moon~n"),
  io:format(" 3. Mars~n"),
  Answer = io:get_line("Which? > "),

  Value = hd(Answer),
  char_to_planemo(Value).

char_to_planemo(Char) ->
  if
    [Char] == "1" -> earth;
    [Char] == "2" -> moon;
    [Char] == "3" -> mars
  end.

get_distance() ->
  Input = io:get_line("How far? (meters) > "),
  Value = string:trim(Input),
  {Distance, _} = string:to_integer(Value),
  Distance.



