-module(udp_server).
-export([init/3, start_link/3, stop/1, loop/1]).

-record(state, {
	server_pid :: pid(),
	count_ecserver :: integer(),
	current_ecserver :: integer(),
	port :: integer(),
	socket :: port()
}).

start_link(Port, Server_pid, Count_ecserver) ->
	{ok , spawn_link(?MODULE, init, [Port, Server_pid, Count_ecserver]) }.

init(Port, Server_pid, Count_ecserver) ->
	io:format("~w",[hello]),
	case gen_udp:open(Port, [ binary, { active, false} ] ) of
		{ok, Socket} ->
			error_logger:info_report({udp_server, self(), Port}),
			loop(#state{
				server_pid = Server_pid,
				count_ecserver = Count_ecserver,
				current_ecserver = 1,
				port = Port,
				socket = Socket
			});
		Error ->
			io:format("~w",Error)
	end.

loop(State) ->
	Data = gen_udp:recv(State#state.socket, 0, 100),
	{ NewState0, Is_exit0 } = do_event(State),
	NewState1 = case Data of
		{ok, { _Address, _Port, Packet } } ->
			error_logger:info_report(Packet),
			Ecserver = get_next_current_ecserver(NewState0),
			Pid = ecserver:pid(State#state.server_pid, Ecserver),
			ecserver:packet(Pid, Packet),
			NewState0#state{current_ecserver = Ecserver };
		{ error, timeout} -> NewState0
	end,
	case Is_exit0 of
		true -> ok;
		_ -> ?MODULE:loop(NewState1)
	end.

get_next_current_ecserver(State) ->
	if
		State#state.current_ecserver >= State#state.count_ecserver -> 1;
		true -> State#state.current_ecserver + 1
	end.

do_event(State) ->
	receive
		stop ->
			{ State , true }
	after 0 ->
		{ State, none }
	end.

stop(Pid) ->
	Pid ! stop.

