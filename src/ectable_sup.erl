-module(ectable_sup).
-export([start_link/4]).

-behaviour(supervisor).
-export([init/1]).

-define(RESTART_TIME, 10000).

init({Server_pid, Count, Tick, Command }) ->
	Childspec = lists:map(fun(Item) ->
		Name = { Server_pid, Item },
		{ Name , { ectable, start_link, [ Server_pid, Item ,  Tick, Command ] }, transient, ?RESTART_TIME, worker, [ ectable, ecsaver ]}
	end, lists:seq(1,Count)),
	{ok , {{simple_one_for_one, 5, 500}, Childspec }}.

start_link(Server_pid, Count, Tick, Command) ->
	supervisor:start_link(?MODULE, { Server_pid, Count, Tick, Command }).

% table_pids(Pid) ->

