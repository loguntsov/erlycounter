-module(udp_server).
-export([init/2, start/2, stop/1, reload/2]).


-record(state, {
	linkers :: queue(), % PID-линкеров, которые решают, куда слать данные
	port :: integer(),
	socket :: port(),
	owner_pid :: pid()
}).

start(Owner_pid, Port) ->
	{ok , spawn_link(?MODULE, init, [Owner_pid, Port]) }.

init(Owner_pid, Port) ->
	{ok, Socket} = gen_udp:open(Port, [ binary, { active, false} ] ),
	udp_server_sup:event_child_start(Owner_pid),
	loop(#state{
		linkers = queue:from_list([ ]),
		port = Port,
		socket = Socket,
		owner_pid = Owner_pid
	}).

loop(State) ->
	Data = gen_udp:recv(State#state.socket, 0, 100),
	{ NewState0, Is_exit0 } = do_event(State),
	{ NewState1, Is_exit1 } = case Data of
		{ok, { _Address, _Port, Packet } } ->
			error_logger:info_report(Packet),
			case queue:out(State#state.linkers) of
				{{ value, Pid }, Linkers } ->
					Linkers1 = queue:in(Pid, Linkers),
					Pid ! { packet, Packet },
					{ NewState0#state{ linkers = Linkers1 }, Is_exit0};
				{ empty, _Linkers } -> { NewState0, Is_exit0 }
			end;
		{ error, timeout} -> { NewState0, Is_exit0 }
	end,
	case Is_exit1 of
		true -> ok;
		_ -> loop(NewState1)
	end.

do_event(State) ->
	receive
		stop ->
			{ State , true };
		{ reload_queue , Linkers } ->
			{State#state{ linkers = Linkers }, none }
	after 0 ->
		{ State, none }
	end.

stop(Pid) ->
	Pid ! stop.

reload(Pid, Linkers) ->
	Pid ! { reload_queue, Linkers }.

