%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2023 16:00
%%%-------------------------------------------------------------------
-module(listdrop).
-author("renevanhoek").

%% API
-export([falls/1]).

falls(List) -> falls(List,[]).

falls([], Results) -> lists:reverse(Results);
falls([Head|Tail], Results) -> falls(Tail, [drop:fall_velocity(Head) | Results]).