-module(kiwi_http_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
 
init(_Type, Req, _Opts) ->
  {ok, Req, undefined_state}.
 
handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello World!">>, Req),
  {ok, Req2, State}.
 
terminate(_Reason, _Req, _State) ->
  ok.