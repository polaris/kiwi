-module(kiwi_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0,
         insert/2,
         lookup/1,
         delete/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

insert(Key, Value) ->
  gen_server:cast(?SERVER, {insert, {Key, Value}}).

lookup(Key) ->
  gen_server:call(?SERVER, {lookup, Key}).

delete(Key) ->
  gen_server:cast(?SERVER, {delete, Key}).

init([]) ->
  case kiwi_db:init() of
    ok ->
      {ok, []};
    {error, Reason} ->
      {stop, {error, Reason}}
  end.

handle_call({lookup, Key}, _From, State) ->
  case kiwi_db:lookup(Key) of
    {ok, Value} ->
      {reply, {ok, Value}, State};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end.

handle_cast(stop, State) ->
  ok = kiwi_db:deinit(),
  {stop, normal, State};
handle_cast({insert, {Key, Value}}, State) ->
  kiwi_db:insert(Key, Value),
  {noreply, State};
handle_cast({delete, Key}, State) ->
  kiwi_db:delete(Key),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-ifdef(TEST).

db_test() ->
  {Result, _Pid} = kiwi_server:start_link(),
  ?assertEqual(ok, Result),
  ?assertEqual({error, not_found}, kiwi_server:lookup("foo")),
  ?assertEqual(ok, kiwi_server:insert("foo", "bar")),
  ?assertEqual({ok, "bar"}, kiwi_server:lookup("foo")),
  ?assertEqual(ok, kiwi_server:delete("foo")),
  ?assertEqual({error, not_found}, kiwi_server:lookup("foo")).

-endif.

