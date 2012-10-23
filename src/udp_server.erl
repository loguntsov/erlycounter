-module(udp_server).
-export([start_link/2]).
-export([socket_accept/2]).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {
	tables :: tuple() % список обслуживаемых супервайзеров
}).

start_link(Socket, Tables) ->
	gen_server:start_link(?MODULE, {Socket, Sups}, []).

init({Socket, Tables}) ->
	spawn_link(?MODULE, socket_accept, [self(), Socket]),
	{ok,#state{tables = Tables}}
.

socket_accept(Pid, Socket) ->
	error_logger:info_report({ udp_server, self()}),
	error_logger:info_report({proc_info, erlang:process_info(Pid)}),
	socket_loop(Pid, Socket),
	end
.

% Прием сообщений и преобразование их в сообщения для gen_server
socket_loop(Pid, Socket) ->
	inet:setopts(Socket,[{active,once}]),
	receive
		{ udp, Socket, Bin } ->
			Counters = binary:split(Bin, [ <<9>>, <<10>>, <<13>> ]),
			lists:map(fun(Counter) ->
				[ Key, Step | Other ] = binary:split(Counter, [ <<" ">> ])
				ok = gen_server:cast(Pid, { step, Key, Value })
			end
			send_server(Socket, Pid, {get, trim(Section)}),
			socket_loop(Pid, Socket);
		{udp_closed, Socket } -> ok
	end
.

handle_call({ step, Key, Value }, State) ->
	Pid = element(erlang:hash(Key, tuple_size(State#state.tables)), State#state.tables)
	ectable:step(Pid, Key, lists:list_to_integer(binary:binary_to_list(Value))),
	{ noreply, State }
.

terminate(Reason, _State) ->
	error_logger:info_report({ tcp_server_terminate, Reason}),
	ok.

handle_cast({ accept, _Socket } , State) -> {noreply, State};

handle_cast( _ , State) ->
	{noreply, State}.

handle_info( _ , State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> { ok, State }.


