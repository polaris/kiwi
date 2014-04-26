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
  case get_value(Req) of
    {ok, _Key, Body, Req2} ->
      {Body, Req2, State};
    {error, not_found, Req2} ->
      {ok, Req3} = cowboy_req:reply(404,
                                    [{<<"content-type">>, <<"text/html">>}],
                                    <<"not found">>,
                                    Req2),
      {halt, Req3, State}
  end.

value_to_json(Req, State) ->
  case get_value(Req) of
    {ok, Key, Value, Req2} ->
      Body = jiffy:encode({[{list_to_binary(Key), list_to_binary(Value)}]}),
      {Body, Req2, State};
    {error, not_found, Req2} ->
      {ok, Req3} = cowboy_req:reply(404,
                                    [{<<"content-type">>, <<"application/json">>}],
                                    <<"{\"error\":\"not found\"}">>,
                                    Req2),
      {halt, Req3, State}
  end.

get_value(Req) ->
  {Value, Req2} = cowboy_req:binding(key, Req),
  Key = binary_to_list(Value),
  case kiwi_server:lookup(Key) of
    {ok, Val} ->
      {ok, Key, Val, Req2};
    {error, not_found} ->
      {error, not_found, Req2}
  end.
