%%%-------------------------------------------------------------------
%%% @author renevanhoek
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2023 18:56
%%%-------------------------------------------------------------------
-module(count).
-author("renevanhoek").

%% API
-export([countdown/1, countup/1, factorial/1, factorial_tailr/1]).

countdown(Number) when Number > 0 ->
  io:format("~w~n", [Number]),
  countdown(Number - 1);

countdown(_Number) ->
  io:format("Done~n").

countup(Limit) ->
  countup(0, Limit).

countup(Number, Limit) when Number =< Limit ->
  io:format("~w~n", [Number]),
  countup(Number + 1, Limit);

countup(_Number, _Limit) ->
  io:format("Done~n").

% This is the traditional recursive implementation. The result is calculated after leaving the recursive call: After unwinding of the
% stack frame. The interpreter has to create a stack-frame for each call in order to keep the state of the function.
% Possible stack-overflow.
factorial(Number) when Number > 1 ->
  Number * factorial(Number - 1);

factorial(_Number) when _Number == 1 ->
  1.

% This is the tail-recursive implementation. The result is calculated at the start before entering the
% recursive call. The result so fas is passed along into each recursive call. Since it is the last call, the interpreter
% does not have to create a stack-frame for each call. It can GOTO the start of the function with updated parameters.
% No stack-overflow.
factorial_tailr(Number) ->
  factorial_tailr(1, Number, 1).

factorial_tailr(Current, Number, Result) when Current =< Number ->
  V = Current * Result,
  factorial_tailr(Current + 1, Number, V);

factorial_tailr(_Current, _Number, Result) ->
  Result.



