%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2023 15:49
%%%-------------------------------------------------------------------
-module(combined).
-author("renevanhoek").

%% API
-export([height_to_mph/1]). % there will be more soon!

%%% combines logic from other modules into a convenience function.
height_to_mph(Meters) ->  convert:mps_to_mph(drop:fall_velocity({earth, Meters})).
