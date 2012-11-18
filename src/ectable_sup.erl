-module(ectable_sup).
-export([start_link/4]).

-behaviour(supervisor).
-export([init/1]).

-define(RESTART_TIME, 10000).

init({Server_pid, Count, Tick, Command }) ->
	Childspecs = lists:map(fun(Item) ->
		{ Item , { ectable, start_link, [ Server_pid, Item ,  Tick, Command ] }, transient, ?RESTART_TIME, worker, dynamic}
	end, lists:seq(1,Count)),
	{ok , {{ one_for_one, 5, 500}, Childspecs }}.

start_link(Server_pid, Count, Tick, Command) ->
	{ok, Pid} = supervisor:start_link(?MODULE, { Server_pid, Count, Tick, Command }),
	{ok, Pid}.


