-module(kiwi).

-export([start/0,
         stop/0]).

start() ->
  ok = mnesia:start(),
  application:start(?MODULE).

stop() ->
  applicatio:stop(?MODULE),
  mnesia:stop().
