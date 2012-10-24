-module(udp_server_sup).

-export([init/1, loop/1, start_link/2, reload/2, event_child_start/1]).

-record(state, {
    linkers :: [ pid() ],
    supervisor :: pid()
}).

init({Owner_pid, Port_from, Port_to}) ->
	{ok, {{ one_for_one, 5, 500 } , lists:map(fun(Port) ->
		{ Port, { udp_server, start, [ Owner_pid, Port ] }, permanent, 2000, worker, [ udp_server ] }
	end, lists:seq(Port_from, Port_to)) }}.

start_link(Port_from, Port_to) ->
	Pid = self(), % spawn_link(?MODULE, loop, [ #state{
%		supervisor = none,
%		linkers = queue:from_list([ ])
%	}]),
	{ok, Sup_pid } = supervisor:start_link(?MODULE, { Pid, Port_from, Port_to }),
	% Pid ! { supervisor, Sup_pid },
	{ ok , Pid }.

loop(State) ->
	receive
		{ reload , Linkers } ->
			Queue = queue:from_list(Linkers),
			lists:map(fun(Child) ->
				udp_server:reload(element(2, Child), Queue)
			end, supervisor:which_children(State#state.supervisor)),
			loop(State#state{
				linkers = Queue
			});
		stop ->
			lists:map(fun(Child) ->
				udp_server:stop(element(2, Child))
			end, supervisor:which_children(State#state.supervisor));
		{ supervisor , Sup_pid } -> loop(State#state{ supervisor = Sup_pid });
		{ child_start, Pid } when State#state.supervisor =/= none ->
			udp_server:reload(Pid, State#state.linkers),
			loop(State);
		_Any -> loop(State)
	end.

reload(Pid, Linkers) ->
	Pid ! { reload, Linkers}.

event_child_start(Pid) ->
	Pid ! { child_start, self() }.

