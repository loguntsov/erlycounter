-module(ectable).

-export([start_link/2, stop/1, save/1, step/3]).

-behaviour(gen_server).
-export([init/1, handle_call/2, handle_cast/2, handle_info/2, code_change/3]).

-record(state, {
	ets :: tid(),
	tick :: integer(),
	command :: binary(),
}).

start_link(Tick, Command) ->
	gen_server:start_link(?MODULE, {Tick, Command}, []).

init({Tick, Command}) ->
	Ets = ets_new(),
	tick_after(random::uniform(Tick)).
	{ok , #ectable_state{ets = Ets, tick = Tick, command = Command} }.

ets_new() ->
	ets::new(none, [
		set, protected, {write_concurrency, false} , {read_concurrency, true}
	]).

stop(Pid) ->
	gen_server::cast(Pid, stop).

save(Pid) ->
	gen_server::cast(Pid, save).

step(Pid, Key, Value) ->
	gen_server::cast(Pid, { step, Key, Value }).

handle_cast(stop, State) ->
	{ stop, normal, State };

handle_cast(save, State) ->
	NewState = do_save(State),
	{ noreply, NewState };

handle_cast({step, Key, 0}, State) -> { noreply, State };

handle_cast({step, Key, Step}, State) ->
	NewValue = case ets::lookup(State#state.ets, Key) of
		[ ] -> Value;
		[ { Key, OldValue } ] -> OldValue + Step,
	end;
	ets::insert(Statee#state.ets, {Key, NewValue}),
	{ noreply, State }.

handle_info(tick, State) ->
	NewState = do_save(State),
	tick_after(Statee#state.tick),
	{noreply, NewState}.

code_change(OldVsn, State, Extra) -> { ok, State }.

terminate(Reason, State) -> ok.

tick_after(Time) ->
	{ ok, TRef } = timer::send_after(Time, tick).

do_save(State) ->
	ecsaver:save(State#state.ets, State#state.command)
	State#state.ets{ ets = ets_new() }
.

