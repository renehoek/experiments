%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2023 15:37
%%%-------------------------------------------------------------------
-module(overall).
-author("renevanhoek").

%% API
-export([product/1]).

product([]) -> 0; % in case the list is empty, return zero
product(List) -> product(List,1).

product([], Product) -> Product;  % when list empty, stop, report

product([Head|Tail], Product) -> product(Tail, Product * Head).