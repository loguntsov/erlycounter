-module(udp_server_sup).

-export([init/1, start_link/4]).

init({Server_pid, Count_ecservers, Port_from, Port_to}) ->
	{ok, {{ one_for_one, 5, 500 } , lists:map(fun(Port) ->
		{ Port, { udp_server, start, [ Port, Server_pid, Count_ecservers ] }, permanent, 2000, worker, [ udp_server ] }
	end, lists:seq(Port_from, Port_to)) }}.

start_link(Server_pid, Count_ecservers, Port_from, Port_to) ->
	supervisor:start_link(?MODULE, { Server_pid, Count_ecservers, Port_from, Port_to }).

