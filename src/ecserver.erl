-module(ecserver).

-export([start_link/3, packet/2, pid/2]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {
	server_pid :: pid(),
	count :: integer()
}).

init({Server_pid, Number, Count_ectables}) ->
	gproc:add_local_name({ecserver, Server_pid, Number}),
	{ok, #state{
		server_pid = Server_pid,
		count = Count_ectables
	}}.

start_link(Server_pid, Number, Count_ectables) ->
	gen_server:start_link(?MODULE, {Server_pid, Number, Count_ectables}).

pid(Server_pid, Number) ->
	gproc:lookup_local_name({ecserver, Server_pid, Number}).

packet(Pid, Packet) ->
	gen_server:cast(Pid, {packet, Packet}).

handle_cast({packet, Packet }, State) ->
	lists:map(fun(Pair) ->
		case binary:split(Pair, <<" ">>, [global, trim] ) of
			[ Key, Value | _ ] ->
				List = binary:binary_to_list(Value),
				try list_to_integer(List) of
					A -> send(State, Key, A)
				catch
					badarg: _ ->
						try list_to_float(List) of
							B -> send(State, Key, B)
						catch
							badarg: _ -> 0
						end
				end,
				ok;
			_ -> ok
		end
	end, binary:split(Packet, <<10>>, [ global ])),
	{ noreply, State };

handle_cast( _, State) -> { noreply, State }.

handle_call( _, _ , State) -> { noreply, State }.

handle_info( _, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> { ok, State }.

terminate(_Reason, _State) -> ok.

send(State, Key, Value) ->
	[ Subkey | _ ]  = binary:split(Key, <<"@">> ),
	Number = erlang:phash(Subkey, State#state.count),
	Pid = ectable:pid(State#state.server_pid, Number),
	ectable:step(Pid, Key, Value).
