-module(ecsaver).

-export([save/2]).

-behaviour(gen_server).

-export([init/1, handle_call/2, handle_cast/2, handle_info/2, code_change/3]).

-record(state, {
	owner_pid :: pid()
}).

save(Ets,Command) ->
	Pid = gen_server:start_link(?MODULE, { self() }, []),
	ets::give_away(Ets, Pid, { cast, Command } ).


init({Owner_pid}) ->
	{ ok ,
		#state{
			owner_pid = Owner_pid,
		}
	}
.

handle_info( {'ETS-TRANSFER', Ets ,State#state.owner_pid, { Method, Command }, State) ->
	do_save(Ets, Command),
	{stop, normal, State}


handle_call( { save, Ets, Command } ) ->
	do_save(Ets, Command),
	{ stop, normal, State }
.

do_save(Ets, Command) ->
	Size = ets:info(Ets, size),
	if
		Size > 0 ->
			Port = erlang:open_port({spawn, Command}, [ stream, use_stdio, binary ]),
			ets:foldl(fun( { Key, Value } , Text ) ->
				true = port_command(Port, <<Key/binary, 32 , binary:list_to_binary(integer_to_list(Value))/binary>> ,  10 >>),
				ok
			end, none, Ets),
			port_close(Port),
			ok;
		Size == 0 -> ok
	end
.
