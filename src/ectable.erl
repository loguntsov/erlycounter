-module(ectable).

-export([start_link/4, stop/1, save/1, step/3, pid/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {
	ets :: integer(),
	tick :: integer(),
	command :: binary(),
	server_pid :: pid(),
	number :: integer()
}).

start_link( Server_pid, Number, Tick, Command ) ->
	gen_server:start_link(?MODULE, {Server_pid, Number, Tick, Command}, []).

init({Server_pid, Number, Tick, Command}) ->
	error_logger:info_report({ectable_server_open, Number}),
	gproc:add_local_name({ectable, Server_pid, Number}),
	Ets = ets_new(),
	tick_after(random:uniform(Tick)),
	{ok , #state{
		ets = Ets,
		tick = Tick,
		command = Command,
		server_pid = Server_pid,
		number = Number
	}}.

pid(Server_pid, Number) ->
	gproc:lookup_local_name({ectable, Server_pid, Number}).

ets_new() ->
	ets:new(none, [
		set, protected, {write_concurrency, false} , {read_concurrency, false}
	]).

stop(Pid) ->
	gen_server:cast(Pid, stop).

save(Pid) ->
	gen_server:cast(Pid, save).

step(Pid, Key, Value) ->
	gen_server:cast(Pid, { step, Key, Value }).

handle_cast(stop, State) ->
	{ stop, normal, State };

handle_cast(save, State) ->
	NewState = do_save(State),
	{ noreply, NewState };

handle_cast({step, _Key, 0}, State) -> { noreply, State };

handle_cast({step, Key, Step}, State) ->
	error_logger:info_report({increment, Key, Step}),
	if
		Step < 0 ->
			try (ets:update_counter(State#state.ets, Key, {2, Step, 0,0})) of
				A -> A
			catch
				error: _ -> 0
			end;
		true ->
			try (ets:update_counter(State#state.ets, Key, {2, Step})) of
				A -> A
			catch
				error: _ ->
					ets:insert(State#state.ets, { Key, Step })
			end
	end,
	{ noreply, State };

handle_cast( _, State ) -> { noreply, State }.

handle_info(tick, State) ->
	NewState = do_save(State),
	tick_after(State#state.tick),
	{noreply, NewState}.

handle_call( _ , _ , State) -> { noreply, State }.



code_change(_OldVsn, State, _Extra) -> { ok, State }.

terminate(_Reason, _State) -> ok.

tick_after(Time) ->
	{ ok, _TRef } = timer:send_after(Time, tick).

do_save(State) ->
	ecsaver:save(State#state.ets, State#state.command),
	State#state{ ets = ets_new() }
.

