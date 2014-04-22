-module(kiwi).

-export([start/0,
         stop/0]).

start() ->
  ok = mnesia:start(),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(crypto),
  ok = application:start(cowboy),
  application:start(?MODULE).

stop() ->
  applicatio:stop(?MODULE),
  mnesia:stop().
