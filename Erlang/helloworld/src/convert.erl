%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2023 15:34
%%%-------------------------------------------------------------------
-module(convert).
-author("renevanhoek").

%% API
-export([mps_to_mph/1, mps_to_kph/1]).


mps_to_mph(Mps) -> 2.23693629 * Mps.

mps_to_kph(Mps) -> 3.6 * Mps.