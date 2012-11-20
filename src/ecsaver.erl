-module(ecsaver).

-export([save/2]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,terminate/2]).
-export([pool_start/1]).

-behaviour(poolboy_worker).

-export([start_link/1]).

pool_start(Count) ->
	poolboy:start_link([
		{name, {local, ecsaver_pool }},
		{size, Count},
		{max_overflow, Count},
		{worker_module, ecsaver}
		], []).

save(Ets,Command) ->
	case poolboy:checkout(ecsaver_pool, false) of
		full -> false;
		Worker ->
			ets:give_away(Ets, Worker, { cast, Command } ),
			true
	end.

start_link(Arg) ->
	gen_server:start_link(?MODULE, Arg, []).

init(_State) ->
	{ ok , {} }.

handle_info( {'ETS-TRANSFER', Ets , _From, { cast, Command }}, State) ->
	do_save(Ets, Command),
	{noreply, State};

handle_info( _, State) -> {noreply, State}.

handle_cast( _, State ) -> {noreply, State}.

handle_call( _, _, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> { ok, State }.

terminate(_Reason, _State) -> ok.

do_save(Ets, Command) ->
	do_save(Ets,Command, ets:info(Ets, size)).

do_save(_Ets, _Command, 0) -> ok;

do_save(Ets, Command, Size) when Size > 0 ->
	Port = erlang:open_port({spawn, binary_to_list(Command)}, [ stream, use_stdio, binary ]),
	ok = ets:foldl(fun( { Key, Value } , _Text ) ->
		Val = list_to_binary(integer_to_list(Value)),
		true = port_command(Port, <<Key/binary, 32 , Val/binary,  10 >>),
		ok
	end, none, Ets),
	port_close(Port),
	poolboy:checkin(ecsaver_pool, self()),
	ok.
