-module(erlycounter_app).

-behaviour(application).

%% application callbacks
-export([start/0, start/2, stop/1]).

-behaviour(supervisor).

-export([init/1]).

start() ->
	application:start(gproc),
	application:start(erlycounter),
	ok.

start(_Type, _StartArgs) ->
	{ok, [Options]} = file:consult("erlycounter.conf"),
	{ok, Pid} = supervisor:start_link(?MODULE, Options),
	io:format("Erlycounter started"),
	{ok, Pid}.

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%----------------------------------------------------------------------
stop(_State) -> ok.

init(Options) ->
	{tables, Tables} = proplists:lookup(tables, Options), % integer
	{servers, Servers} = proplists:lookup(servers, Options), % integer
	{command, Command} = proplists:lookup(command, Options), % binary
	{port_range, Port_range} = proplists:lookup(port_range, Options), % {port_from, port_to}
	{save_process, Save_process} = proplists:lookup(save_process, Options), %integer
	{save_time, Save_time} = proplists:lookup(save_time, Options), %integer
	{pid_file, Pid_file} = proplists:lookup(pid_file, Options), %binary

	file:write_file(Pid_file, os:getpid()),

	{ ok, {{one_for_one, 5, 1000}, [
		{ecsaver_pool, { ecsaver, pool_start, [ Save_process ] }, permanent, 5000 , supervisor, [] },
		{ectable_sup, { ectable_sup, start_link, [ server_id, Tables, Save_time*1000, Command ] }, permanent, 5000 , supervisor, [] },
		{ecserver_sup, { ecserver_sup, start_link, [ server_id, Servers, Tables ] }, permanent, 5000 , supervisor, dynamic },
		{udp_server_sup, { udp_server_sup, start_link, [ server_id, Servers, element(1,Port_range), element(2, Port_range)] }, permanent, 5000 , supervisor, dynamic }
	]}}.

