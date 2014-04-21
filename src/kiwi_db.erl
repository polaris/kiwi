-module(kiwi_db).

-export([init/0,
         deinit/0,
         insert/2,
         lookup/1,
         delete/1]).

-record(key_to_value, {key, value}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
  ok = mnesia:start(),
  case mnesia:create_table(key_to_value, [{attributes, record_info(fields, key_to_value)}]) of
    {atomic, ok} ->
      ok;
    {aborted, {already_exists, key_to_value}} ->
      ok;
    {aborted, Reason} ->
      {error, Reason}
  end.

deinit() ->
  stopped = mnesia:stop(),
  ok.


insert(Key, Value) ->
  mnesia:dirty_write(#key_to_value{key=Key, value=Value}).

lookup(Key) ->
  case mnesia:dirty_read(key_to_value, Key) of
    [{key_to_value, Key, Value}] ->
      {ok, Value};
    [] ->
      {error, not_found}
  end.

delete(Key) ->
  mnesia:dirty_delete(key_to_value, Key).

-ifdef(TEST).

db_test() ->
  ?assertEqual(ok, kiwi_db:init()),
  ?assertEqual({error, not_found}, kiwi_db:lookup("foo")),
  ?assertEqual(ok, kiwi_db:insert("foo", "bar")),
  ?assertEqual({ok, "bar"}, kiwi_db:lookup("foo")),
  ?assertEqual(ok, kiwi_db:delete("foo")),
  ?assertEqual({error, not_found}, kiwi_db:lookup("foo")),
  ?assertEqual(ok, kiwi_db:deinit()).

-endif.
