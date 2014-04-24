-module(kiwi_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
      %% {URIHost, list({URIPath, Handler, Opts})}
      {'_', [{"/:key", kiwi_http_handler, []}]}
  ]),
  %% Name, NbAcceptors, TransOpts, ProtoOpts
  cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),
  kiwi_sup:start_link().

stop(_State) ->
  ok.
