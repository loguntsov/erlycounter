-module(ecserver_sup).
-export([start_link/3]).

-behaviour(supervisor).
-export([init/1]).

-define(RESTART_TIME, 10000).

init({Server_pid, Count, Count_ectables}) ->
	Childspec = lists:map(fun(Item) ->
		{ Item , { ecserver, start_link, [ Server_pid, Item ,  Count_ectables ] }, transient, ?RESTART_TIME, worker, [ ecserver, ecserver_sup ]}
		end, lists:seq(1,Count)),
	{ok , {{one_for_one, 5, 500}, Childspec }}.

start_link(Server_pid, Count, Count_ectables) ->
	supervisor:start_link(?MODULE, { Server_pid, Count, Count_ectables }).



