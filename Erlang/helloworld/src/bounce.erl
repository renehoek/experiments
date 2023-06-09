%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2023 16:23
%%%-------------------------------------------------------------------
-module(bounce).
-author("renevanhoek").

%% API
-export([report/1]).

report(Count) ->
  NewCount = receive
    X -> io:format("Received: #~p ~p~n", [Count, X]),
      Count + 1
  end,
  report(NewCount).

