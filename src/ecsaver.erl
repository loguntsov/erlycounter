-module(ecsaver).

-export([save/2]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,terminate/2]).

-record(state, {
	owner_pid :: pid()
}).

save(Ets,Command) ->
	Pid = gen_server:start_link(?MODULE, { self() }, []),
	ets:give_away(Ets, Pid, { cast, Command } ).


init({Owner_pid}) ->
	{ ok ,
		#state{
			owner_pid = Owner_pid
		}
	}
.

handle_info( {'ETS-TRANSFER', Ets , Owner_pid, { _Method, Command }}, State) when Owner_pid == State#state.owner_pid ->
	do_save(Ets, Command),
	{stop, normal, State};

handle_info( _, State) -> {noreply, State}.

handle_cast( _, State ) -> {noreply, State}.

handle_call( _, _, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> { ok, State }.

terminate(_Reason, _State) -> ok.

do_save(Ets, Command) ->
	do_save(Ets,Command, ets:info(Ets, size)).

do_save(_Ets, _Command, 0) -> ok;

do_save(Ets, Command, Size) when Size > 0 ->
	Port = erlang:open_port({spawn, Command}, [ stream, use_stdio, binary ]),
	ok = ets:foldl(fun( { Key, Value } , _Text ) ->
		Val = binary:list_to_binary(integer_to_list(Value)),
		true = port_command(Port, <<Key/binary, 32 , Val/binary,  10 >>),
		ok
	end, none, Ets),
	port_close(Port),
	ok.
