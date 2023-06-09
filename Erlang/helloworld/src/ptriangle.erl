%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2023 17:28
%%%-------------------------------------------------------------------
-module(ptriangle).
-author("renevanhoek").

%% API
-export([add_row/1, triangle/2]).

add_row(Source) ->
  add_row(Source, 0, []).

add_row([], Last, Target) ->
  %io:format("Last: ~w Target: ~w ~n", [Last, Target]),
  F = [Last | Target],
  F;

add_row(Source, Last, Target) ->
  %io:format("Row: ~w Last: ~w Result: ~w ~n", [Row, Last, Target]),
  [Head | Tail] = Source,
  New = Head + Last,
  F = [New | Target],
  add_row(Tail, Head, F).

% 	    Row		    Last		Result		Head		Tail		      New		F
% 23:1	[1,3,3,1]	0		    []		    1		    [3,3,1]		    1		  [1]
% 23:2	[3,3,1]		1		    [1]		    3		    [3,1]		      4		  [4,1]
% 23:3	[3,1]		  3		    [4,1]		  3		    [1]		        6		  [6,4,1]
% 23:4	[1]		    3		    [6,4,1]		1		    []		        4		  [4,6,4,1]
% 18:5			      1		    [4,6,4,1]					        		        [1,4,6,4,1]

triangle(Source, Limit) ->
  triangle(Source, 0, Limit).

triangle(Source, Count, Limit) when Count < Limit ->
  [LastRow | _] = Source,

  NewRow = add_row(LastRow),
  F = [NewRow | Source],
  triangle(F, Count + 1, Limit);

triangle(Source, _, _) ->
  lists:reverse(Source).

