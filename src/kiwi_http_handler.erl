-module(kiwi_http_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([value_to_text/2]).
-export([value_to_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[
    {<<"text/plain">>, value_to_text},
    {<<"text/html">>, value_to_text},
    {<<"application/json">>, value_to_json}
  ], Req, State}.

value_to_text(Req, State) ->
  {_Key, Body, Req2} = get_value(Req),
  {Body, Req2, State}.

value_to_json(Req, State) ->
  {Key, Value, Req2} = get_value(Req),
  Body = jiffy:encode({[{list_to_binary(Key), list_to_binary(Value)}]}),
  {Body, Req2, State}.

get_value(Req) ->
  {Value, Req2} = cowboy_req:binding(key, Req),
  Key = binary_to_list(Value),
  {ok, Val} = kiwi_server:lookup(Key),
  {Key, Val, Req2}.
